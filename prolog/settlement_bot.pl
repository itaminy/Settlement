:- module(settlement_bot, [run_bot/6, choose_resource/3]).
:- encoding(utf8).

:- use_module(settlement_client).
:- use_module(settlement_dcg).
:- use_module(settlement_knowledge, except([resource/3])).
:- reexport(settlement_strategy, [choose_resource/3]).
:- use_module(settlement_strategy, [choose_action/3]).
:- use_module(library(error)).
:- use_module(library(time)).
:- use_module(library(uuid)).

:- meta_predicate run_bot(+, +, +, 0, -, -).
:- initialization(main, main).

% Stop is a semidet callback. Game command attempts consume actions.
% Protocol setup, snapshots, waiting and cleanup do not.
run_bot(Id, Auth, limits(Max, Seconds), Stop, Credentials, Result) :-
    must_be(nonneg, Max),
    must_be(number, Seconds),
    ( Seconds > 0 -> true ; domain_error(positive_time, Seconds) ),
    with_mutex(settlement_bot,
               run_session(Id, Auth, Max, Seconds, Stop, Credentials, Result)).

run_session(Id, Auth, Max, Seconds, Stop, Credentials, Result) :-
    reset_knowledge,
    get_time(Now),
    Deadline is Now + Seconds,
    Counter = counter(0),
    Context = run(Id, Max, Deadline, Stop, Counter),
    catch(catch(run_connections(Context, Auth, plan_state([], none, 0), Credentials, Reason),
                bot_stop(Why), Reason = Why),
          time_limit_exceeded, Reason = time_limit),
    arg(1, Counter, Actions),
    Result = stopped(Reason, Actions).

connected(Id, Max, Deadline, Stop, Counter, Reason, In, Out) :-
    connected(Id, Max, Deadline, Stop, Counter, plan_state([], none, 0), Reason, In, Out).

run_connections(Context, Auth, State, Credentials, Reason) :-
    Context = run(Id, Max, Deadline, Stop, Counter),
    control_time(Deadline, Time),
    with_text_session(Id, Auth, Time, Credentials,
                      connected(Id, Max, Deadline, Stop, Counter, State, Outcome)),
    ( Outcome = change_connection(Tool, NextState) ->
        control_time(Deadline, CraftTime),
        with_edn_session(Id, login(Credentials), CraftTime, _,
                         craft_in_edn(Context, Tool)),
        run_connections(Context, login(Credentials), NextState, Credentials, Reason)
    ; Reason = Outcome
    ).

craft_in_edn(run(Id, Max, Deadline, Stop, Counter), Tool, In, Out) :-
    Session = session(In, Out, Deadline, Stop, Max, Counter, ready),
    consume_action(Session),
    remaining_time(Session, Time),
    call_with_time_limit(Time, craft_tool(Id, Tool, In, Out, Reply)),
    ( Reply == ok -> log('изготовление подтверждено: ~w', [Tool])
    ; Reply = error(Code), log('изготовление отклонено: ~s', [Code])
    ).

connected(Id, Max, Deadline, Stop, Counter, PlanState, Reason, In, Out) :-
    Session = session(In, Out, Deadline, Stop, Max, Counter, ready),
    catch(catch(( check_action(Session),
                  refresh(Session, Id),
                  subscribe(Session, Id),
                  play(Session, Id, PlanState, Reason)
                ), bot_stop(Why), Reason = Why),
          time_limit_exceeded, Reason = time_limit).

subscribe(Session, Id) :-
    exchange(Session, "подпишись на события", Reply),
    ( Reply = error("resync-required", _) ->
        refresh(Session, Id),
        exchange(Session, "подпишись на события", Retry),
        expect_reply(Retry, subscribe_events(_, _))
    ; expect_reply(Reply, subscribe_events(_, _))
    ).

refresh(Session, Id) :-
    exchange(Session, "покажи мир с версией", Reply),
    expect_reply(Reply, world_versioned(Version, _, _, _)),
    ( load_snapshot(reply(1, Reply)), bot_identity(Id, _) ->
        resource_counts(Wood, Stone),
        current_score(Blue, Red),
        log('снимок v=~d, личный запас: дерево=~d камень=~d, счёт: blue=~d red=~d',
            [Version, Wood, Stone, Blue, Red])
    ; throw(settlement_error(invalid_bot_snapshot))
    ).

play(Session, Id, State, Reason) :-
    State = plan_state(Excluded, _, ReadyAt),
    ( match_state(finished, _) -> true ; check_action(Session) ),
    remaining_time(Session, Time),
    get_time(Now),
    Cooldown is max(0, ReadyAt - Now),
    call_with_time_limit(Time, choose_action(Excluded, Cooldown, Plan)),
    execute_plan(Plan, Session, Id, State, Reason).

execute_plan(stop(Reason), _, _, _, Reason).
execute_plan(wait(Seconds), Session, Id, State, Reason) :-
    Delay is min(Seconds, 0.2),
    wait_action(Session, Delay),
    refresh(Session, Id),
    play(Session, Id, State, Reason).
execute_plan(craft(Tool), _, _, plan_state(Excluded, _, ReadyAt),
             change_connection(Tool, plan_state([craft(Tool) | Excluded], none, ReadyAt))).
execute_plan(reposition(Direction, Target), Session, Id,
             plan_state(Excluded, PreviousFailure, ReadyAt), Reason) :-
    movement_command(Direction, Command),
    action(Session, Command, Reply),
    ( Reply = ok(move(X, Y)) ->
        log('шаг ~w подтверждён: (~d, ~d)', [Direction, X, Y]),
        NextExcluded = Excluded,
        Failure = none
    ; Reply = error(Code, _) ->
        log('отказ движения ~w: ~s, перепланирование', [Direction, Code]),
        ( PreviousFailure == Target -> NextExcluded = [Target | Excluded]
        ; NextExcluded = Excluded
        ),
        Failure = Target
    ; throw(settlement_error(unexpected_move_reply))
    ),
    refresh(Session, Id),
    play(Session, Id, plan_state(NextExcluded, Failure, ReadyAt), Reason).
execute_plan(gather(Type, X, Y), Session, Id, plan_state(Excluded, _, ReadyAt), Reason) :-
    Target = gather(Type, X, Y),
    gather(Session, Id, Target, Outcome),
    refresh(Session, Id),
    ( Outcome == gathered -> NextExcluded = Excluded ; NextExcluded = [Target | Excluded] ),
    play(Session, Id, plan_state(NextExcluded, none, ReadyAt), Reason).
execute_plan(Target, Session, Id, plan_state(Excluded, _, ReadyAt), Reason) :-
    building_command(Target, Command, Expected),
    action(Session, Command, Reply),
    building_reply(Target, Expected, Reply, Excluded, NextExcluded, ReadyAt, NextReadyAt),
    refresh(Session, Id),
    play(Session, Id, plan_state(NextExcluded, none, NextReadyAt), Reason).

building_command(build(Kind, X, Y), Command, build(building(_, X, Y, KindText, Team, Id, _, _))) :-
    building_name(Kind, Name),
    atom_string(Kind, KindText),
    bot_identity(Id, Team),
    format(string(Command), 'построй ~s на ~d ~d', [Name, X, Y]).
building_command(attack(UUID), Command, attack_building(Seq, X, Y, UUID, _, _)) :-
    building(UUID, X, Y, _, _, _, _, _),
    command_sequences(Last, _),
    next_sequence(Last, Seq),
    format(string(Command), 'атакуй постройку на ~d ~d id ~s номер ~d', [X, Y, UUID, Seq]).
building_command(repair(UUID), Command, repair_building(Seq, X, Y, UUID, _, _)) :-
    building(UUID, X, Y, _, _, _, _, _),
    command_sequences(_, Last),
    next_sequence(Last, Seq),
    format(string(Command), 'почини постройку на ~d ~d id ~s номер ~d', [X, Y, UUID, Seq]).

building_name(fence, "забор").
building_name(house, "дом").

next_sequence(Last, Seq) :-
    ( Last < 9223372036854775807 -> Seq is Last + 1
    ; throw(settlement_error(building_sequence_exhausted))
    ).

building_reply(Target, Expected, Reply, Excluded, NextExcluded, ReadyAt, NextReadyAt) :-
    ( Reply = ok(Expected) ->
        confirm_building(Expected),
        NextExcluded = Excluded,
        ( Target = attack(_) -> new_cooldown(NextReadyAt) ; NextReadyAt = ReadyAt )
    ; Reply = error(Code, _) ->
        log('действие ~q отклонено: ~s', [Target, Code]),
        ( Target = attack(_), Code == "attack-cooldown" ->
            new_cooldown(NextReadyAt), NextExcluded = Excluded
        ; NextReadyAt = ReadyAt, NextExcluded = [Target | Excluded]
        )
    ; throw(settlement_error(unexpected_building_reply))
    ).

new_cooldown(ReadyAt) :-
    get_time(Now),
    ReadyAt is Now + 1.

confirm_building(build(building(UUID, X, Y, Kind, _, _, _, _))) :-
    string(UUID), !,
    log('постройка подтверждена: ~s (~d, ~d), UUID ~s', [Kind, X, Y, UUID]).
confirm_building(attack_building(Seq, X, Y, _, HP, Destroyed)) :-
    log('атака №~d подтверждена: (~d, ~d), HP=~d, уничтожена=~w', [Seq, X, Y, HP, Destroyed]).
confirm_building(repair_building(Seq, X, Y, _, HP, Restored)) :-
    Restored > 0, !,
    log('ремонт №~d подтверждён: (~d, ~d), HP=~d (+~d)', [Seq, X, Y, HP, Restored]).
confirm_building(_) :-
    throw(settlement_error(invalid_building_confirmation)).

gather(Session, Id, Target, Outcome) :-
    check_action(Session),
    uuid(UUID, [version(4)]),
    atom_string(UUID, Attempt),
    setup_call_cleanup(
        AttemptState = attempt(Attempt, armed),
        start_gather(Session, Id, Target, AttemptState, Outcome),
        cancel_if_armed(Session, AttemptState)).

start_gather(Session, Id, gather(Type, X, Y), State, Outcome) :-
    State = attempt(Attempt, _),
    gather_material(Type, Name, Material, _),
    format(string(Command), 'собери ~s на ~d ~d попытка ~s', [Name, X, Y, Attempt]),
    action(Session, Command, Reply),
    ( Reply = ok(gather_start(progress(Operation, Attempt, Duration, Elapsed))) ->
        log('добыча начата: ~w (~d, ~d), ~d мс', [Type, X, Y, Duration]),
        Target = gather(Type, X, Y),
        refresh(Session, Id),
        gather_progress(Session, Id, Target, Material, State,
                        progress(Operation, Attempt, Duration, Elapsed), Outcome)
    ; Reply = error(Code, _) ->
        nb_setarg(2, State, disarmed),
        gather_rejected(Code, Outcome)
    ; throw(settlement_error(unexpected_gather_start_reply))
    ).

gather_progress(Session, Id, Target, Material, State, Progress, Outcome) :-
    ( gathering_player(Id), Target = gather(Type, X, Y),
      settlement_knowledge:resource(X, Y, Type) ->
        Progress = progress(_, _, Duration, Elapsed),
        Left is max(0, Duration - Elapsed),
        % Server heartbeat expires at 1500 ms. Leave room for network latency.
        Delay is min(0.4, Left / 1000),
        wait_action(Session, Delay),
        ( Left =< 400 ->
            finish_gather(Session, Id, Target, Material, State, Progress, Outcome)
        ; heartbeat(Session, Id, Target, Material, State, Progress, Outcome)
        )
    ; gather_rejected("target-unavailable", Outcome)
    ).

heartbeat(Session, Id, Target, Material, State, Progress, Outcome) :-
    Progress = progress(Operation, Attempt, Duration, _),
    format(string(Command), 'продолжай добычу ~s', [Operation]),
    action(Session, Command, Reply),
    ( Reply = ok(gather_heartbeat(progress(Operation, Attempt, Duration, Elapsed))) ->
        ( needs_resync -> refresh(Session, Id) ; true ),
        gather_progress(Session, Id, Target, Material, State,
                        progress(Operation, Attempt, Duration, Elapsed), Outcome)
    ; Reply = error(Code, _) ->
        gather_rejected(Code, Outcome)
    ; throw(settlement_error(unexpected_gather_heartbeat_reply))
    ).

finish_gather(Session, Id, Target, Material, State, Progress, Outcome) :-
    Progress = progress(Operation, Attempt, Duration, _),
    Target = gather(_, X, Y),
    format(string(Command), 'заверши добычу ~s', [Operation]),
    action(Session, Command, Reply),
    ( Reply = ok(gather_finish(Operation, Material, Amount, X, Y)), Amount > 0 ->
        nb_setarg(2, State, disarmed),
        log('добыча подтверждена: ~w +~d, цель (~d, ~d)', [Material, Amount, X, Y]),
        Outcome = gathered
    ; Reply = error("too-early", progress(Operation, Attempt, Duration, _)) ->
        heartbeat(Session, Id, Target, Material, State, Progress, Outcome)
    ; Reply = error(Code, _) ->
        gather_rejected(Code, Outcome)
    ; throw(settlement_error(unexpected_gather_finish_reply))
    ).

gather_rejected(Code, rejected) :-
    log('добыча отклонена: ~s', [Code]).

cancel_if_armed(Session, attempt(Attempt, Status)) :-
    ( Status == armed ->
        format(string(Command), 'отмени попытку добычи ~s', [Attempt]),
        catch(call_with_time_limit(0.5, cancel_attempt(Session, Command, Attempt)), _, true)
    ; true
    ).

cancel_attempt(Session, Command, Attempt) :-
    Session = session(In, Out, _, _, _, _, State),
    send_line(Out, Command),
    ( State == ready, read_reply(In, ok(gather_cancel(none, Attempt))) ->
        log('отмена добычи подтверждена', [])
    ; true
    ).

action(Session, Command, Reply) :-
    consume_action(Session),
    exchange(Session, Command, Reply).

consume_action(Session) :-
    check_action(Session),
    arg(6, Session, Counter),
    arg(1, Counter, Count),
    Next is Count + 1,
    nb_setarg(1, Counter, Next).

exchange(Session, Command, Reply) :-
    remaining_time(Session, Time),
    Session = session(In, Out, _, _, _, _, _),
    nb_setarg(7, Session, pending),
    call_with_time_limit(Time, (send_line(Out, Command), read_reply(In, Reply))),
    nb_setarg(7, Session, ready).

% Exactly one reader owns the connection. Push frames may precede any reply.
read_reply(In, Reply) :-
    read_line(In, Line),
    parse_frame(Line, Parsed),
    ( Parsed = ok(reply(1, Payload)) ->
        Reply = Payload
    ; Parsed = ok(Event), apply_event(Event) ->
        Event = event(1, Version, _, Changes, Resync),
        log('событие v=~d: ~q resync=~w', [Version, Changes, Resync]),
        read_reply(In, Reply)
    ; throw(settlement_error(invalid_text_frame))
    ).

expect_reply(Reply, Payload) :-
    ( Reply = ok(Payload) -> true
    ; Reply = error(Code, _) -> throw(settlement_error(text_rejected(Code)))
    ; throw(settlement_error(unexpected_text_reply))
    ).

check_action(Session) :-
    Session = session(_, _, _, Stop, Max, Counter, _),
    ( call(Stop) -> throw(bot_stop(requested)) ; true ),
    remaining_time(Session, _),
    arg(1, Counter, Count),
    ( Count < Max -> true ; throw(bot_stop(action_limit)) ).

remaining_time(Session, Time) :-
    arg(3, Session, Deadline),
    control_time(Deadline, Time).

control_time(Deadline, Time) :-
    get_time(Now),
    Time is Deadline - Now,
    ( Time > 0 -> true ; throw(bot_stop(time_limit)) ).

wait_action(Session, Delay) :-
    check_action(Session),
    remaining_time(Session, Time),
    Wait is min(Delay, Time),
    sleep(Wait).

movement_command(up, "иди вверх").
movement_command(down, "иди вниз").
movement_command(left, "иди влево").
movement_command(right, "иди вправо").

gather_material(tree, "дерево", wood, 0).
gather_material(stone, "камень", stone, 1).

log(Format, Args) :-
    format(Format, Args),
    nl,
    flush_output.

main([IdText, MaxText, SecondsText]) :-
    catch(( atom_number(IdText, Id), atom_number(MaxText, Max),
            atom_number(SecondsText, Seconds)
          ), _, fail),
    !,
    nb_setval(settlement_bot_stop, false),
    catch(setup_call_cleanup(
              on_signal(int, Previous, request_stop),
              ( run_bot(Id, register("Бот Settlement"), limits(Max, Seconds),
                        stop_requested, _, Result),
                log('цикл завершён: ~q', [Result])
              ),
              on_signal(int, _, Previous)),
          Error, (print_message(error, Error), halt(1))).
main(_) :-
    format(user_error, 'Запуск: swipl -q -s prolog/settlement_bot.pl -- ID ДЕЙСТВИЯ СЕКУНДЫ~n', []),
    halt(2).

request_stop(_) :- nb_setval(settlement_bot_stop, true).
stop_requested :- nb_current(settlement_bot_stop, true).
