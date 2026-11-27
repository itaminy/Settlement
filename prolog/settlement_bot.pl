:- module(settlement_bot, [run_bot/6, choose_resource/3]).
:- encoding(utf8).

:- use_module(settlement_client).
:- use_module(settlement_dcg).
:- use_module(settlement_knowledge, except([resource/3])).
:- reexport(settlement_strategy, [choose_resource/3]).
:- use_module(settlement_strategy, [choose_action/3, choose_decision/5, immediate_danger/0]).
:- use_module(settlement_strategy_config, [priority/2]).
:- use_module(settlement_pathfinding, [path_to/2]).
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
               setup_call_cleanup(init_pacing,
                   run_session(Id, Auth, Max, Seconds, Stop, Credentials, Result),
                   (nb_delete(settlement_pacing), nb_delete(settlement_decision),
                    nb_delete(settlement_snapshot_summary)))).

delay_setting(Name, Default, Minimum, Seconds) :-
    ( getenv(Name, Value) ->
        ( atom_number(Value, Ms), integer(Ms), between(Minimum, 60000, Ms) -> true
        ; throw(error(domain_error(bot_timing, Name=Value), _))
        )
    ; Ms = Default
    ),
    Seconds is Ms/1000.

init_pacing :-
    delay_setting('SETTLEMENT_BOT_MOVE_MS', 350, 1, Move),
    delay_setting('SETTLEMENT_BOT_ACTION_MS', 700, 700, Action),
    delay_setting('SETTLEMENT_BOT_BUILD_MS', 2000, 1, Build),
    delay_setting('SETTLEMENT_BOT_DECISION_MS', 1000, 1, Decision),
    get_time(Now),
    nb_setval(settlement_decision, decision(none, starting, 0, wait(0))),
    nb_setval(settlement_pacing, pacing(Move,Action,Build,Decision,Now,Now,Now,Now,none)).

pace(Session, Kind) :-
    ( nb_current(settlement_pacing, pacing(_,_,_,_,Move,Action,Build,_,_)) ->
        get_time(Now),
        ( Kind == move -> Ready is max(Move, Build)
        ; Ready is max(Action, Build)
        ),
        ( Ready > Now -> wait_action(Session, Ready-Now) ; true )
    ; true
    ).

stamp(Kind) :-
    ( nb_current(settlement_pacing, Clock) ->
        get_time(Now),
        ( Kind == move -> arg(1,Clock,Delay), Ready is Now+Delay, nb_setarg(5,Clock,Ready)
        ; arg(2,Clock,Delay), Ready is Now+Delay, nb_setarg(6,Clock,Ready),
          ( Kind == build ->
              arg(3,Clock,Pause), Resume is Now+Pause, nb_setarg(7,Clock,Resume)
          ; true )
        )
    ; true
    ).

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
    ( Outcome = change_connection(Tool, plan_state(Rejected, Failure, ReadyAt)) ->
        control_time(Deadline, CraftTime),
        with_edn_session(Id, login(Credentials), CraftTime, _,
                         craft_in_edn(Context, Tool, Reply)),
        ( Reply == ok -> NextRejected = Rejected
        ; Reply = error(Code), reject_target(retry_seconds, Code, craft(Tool), Rejected, NextRejected) ),
        NextState = plan_state(NextRejected, Failure, ReadyAt),
        run_connections(Context, login(Credentials), NextState, Credentials, Reason)
    ; Reason = Outcome
    ).

craft_in_edn(run(Id, Max, Deadline, Stop, Counter), Tool, Reply, In, Out) :-
    Session = session(In, Out, Deadline, Stop, Max, Counter, ready),
    pace(Session, action),
    consume_action(Session),
    remaining_time(Session, Time),
    call_with_time_limit(Time, craft_tool(Id, Tool, In, Out, Reply)),
    ( Reply == ok -> stamp(action), log('изготовление подтверждено: ~w', [Tool])
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
        bot_health(HP),
        log_snapshot(Version, Wood, Stone, Blue, Red, HP)
    ; throw(settlement_error(invalid_bot_snapshot))
    ).

log_snapshot(Version, Wood, Stone, Blue, Red, HP) :-
    Summary = summary(Wood, Stone, Blue, Red, HP),
    ( nb_current(settlement_snapshot_summary, Summary) -> true
    ; nb_setval(settlement_snapshot_summary, Summary),
      log('снимок v=~d: дерево=~d камень=~d, счёт blue=~d red=~d, HP=~d',
          [Version, Wood, Stone, Blue, Red, HP]) ).

play(Session, Id, State, Reason) :-
    State = plan_state(Rejected, Failure, ReadyAt),
    ( match_state(finished, _) -> true ; check_action(Session) ),
    remaining_time(Session, Time),
    get_time(Now),
    Cooldown is max(0, ReadyAt - Now),
    active_rejections(Rejected, Now, Active, Excluded),
    call_with_time_limit(Time, strategic_plan(Session, Excluded, Cooldown, Plan, Unreachable)),
    foldl(reject_target(no_route_retry_seconds, no_route), Unreachable, Active, NextRejected),
    ( Plan == stop(no_reachable_resource), nb_current(settlement_pacing, _), NextRejected \== [] ->
        NextPlan = wait(1)
    ; NextPlan = Plan ),
    execute_plan(NextPlan, Session, Id, plan_state(NextRejected, Failure, ReadyAt), Reason).

strategic_plan(Session, Excluded, Cooldown, Plan, Unreachable) :-
    ( nb_current(settlement_pacing, Clock), match_state(active, _) ->
        arg(8, Clock, Ready), arg(9, Clock, Target), get_time(Now),
        ( Now < Ready, Target \== none, \+ memberchk(Target, Excluded),
          path_to(Target, [Direction|_]),
          can_continue(Target, Direction) ->
            Plan = reposition(Direction, Target), Unreachable = []
        ; ( Now < Ready -> wait_action(Session, Ready-Now) ; true ),
          nb_current(settlement_decision, Previous),
          Previous = decision(OldTarget, _, _, _),
          choose_decision(Excluded, Cooldown, OldTarget, Decision, Unreachable),
          Decision = decision(_, _, _, Plan),
          report_decision(Previous, Decision),
          nb_setval(settlement_decision, Decision),
          get_time(Decided), arg(4,Clock,Interval), Next is Decided+Interval,
          nb_setarg(8,Clock,Next),
          ( Plan = reposition(_, Goal) -> nb_setarg(9,Clock,Goal) ; nb_setarg(9,Clock,none) )
        )
    ; choose_action(Excluded, Cooldown, Plan), Unreachable = []
    ).

can_continue(cell(_, _), Direction) :- !,
    bot_position(X, Y),
    settlement_strategy:safe_initial_step(X, Y, [Direction]).
can_continue(_, _) :- \+ immediate_danger.

report_decision(decision(Old, OldReason, _, _), decision(Target, Reason, Utility, Action)) :-
    ( Old == Target, OldReason == Reason -> true
    ; log('стратегия: действие=~q цель=~q полезность=~d причина=~w предыдущая_цель=~q предыдущая_причина=~w',
          [Action, Target, Utility, Reason, Old, OldReason])
    ).

active_rejections([], _, [], []).
active_rejections([rejected(Target, Until)|Rest], Now, Active, Excluded) :-
    active_rejections(Rest, Now, Tail, Goals),
    ( Until > Now -> Active = [rejected(Target, Until)|Tail], Excluded = [Target|Goals]
    ; Active = Tail, Excluded = Goals ).

reject_target(Setting, Cause, Target, Rejected, Next) :-
    ( memberchk(rejected(Target, _), Rejected) -> Next = Rejected
    ; priority(Setting, Delay), get_time(Now), Until is Now+Delay,
      Next = [rejected(Target, Until)|Rejected],
      log('цель отложена: ~q причина=~w повтор через ~d с', [Target, Cause, Delay])
    ).

execute_plan(stop(Reason), _, _, _, Reason).
execute_plan(wait(Seconds), Session, Id, State, Reason) :-
    Delay is min(Seconds, 0.2),
    wait_action(Session, Delay),
    refresh(Session, Id),
    play(Session, Id, State, Reason).
execute_plan(craft(Tool), _, _, plan_state(Excluded, _, ReadyAt),
             change_connection(Tool, plan_state(Excluded, none, ReadyAt))).
execute_plan(reposition(Direction, Target), Session, Id,
             plan_state(Excluded, PreviousFailure, ReadyAt), Reason) :-
    movement_command(Direction, Command),
    pace(Session, move),
    action(Session, Command, Reply),
    ( Reply = ok(move(X, Y)) ->
        stamp(move),
        integer(X), integer(Y),
        NextExcluded = Excluded,
        Failure = none
    ; Reply = error("bot-cooldown", _) ->
        wait_action(Session, 0.2),
        NextExcluded = Excluded,
        Failure = none
    ; Reply = error(Code, _) ->
        log('отказ движения ~w: ~s, перепланирование', [Direction, Code]),
        ( PreviousFailure == Target ->
            reject_target(retry_seconds, Code, Target, Excluded, NextExcluded)
        ; NextExcluded = Excluded
        ),
        Failure = Target
    ; throw(settlement_error(unexpected_move_reply))
    ),
    refresh(Session, Id),
    play(Session, Id, plan_state(NextExcluded, Failure, ReadyAt), Reason).
execute_plan(gather(Type, X, Y), Session, Id, plan_state(Excluded, _, ReadyAt), Reason) :-
    Target = gather(Type, X, Y),
    pace(Session, action),
    gather(Session, Id, Target, Outcome),
    ( Outcome == gathered -> stamp(action) ; true ),
    refresh(Session, Id),
    ( memberchk(Outcome, [gathered, cooldown, interrupted]) -> NextExcluded = Excluded
    ; reject_target(retry_seconds, Outcome, Target, Excluded, NextExcluded)
    ),
    play(Session, Id, plan_state(NextExcluded, none, ReadyAt), Reason).
execute_plan(Target, Session, Id, plan_state(Excluded, _, ReadyAt), Reason) :-
    building_command(Target, Command, Expected),
    pace(Session, action),
    action(Session, Command, Reply),
    ( Reply = ok(Expected) ->
        ( Target = build(_,_,_) -> stamp(build) ; stamp(action) )
    ; Reply = error("bot-cooldown", _) -> wait_action(Session, 0.2)
    ; true ),
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
building_command(attack_bandit(UUID), Command, attack_bandit(Seq, X, Y, UUID, _, _)) :-
    bandit(UUID, X, Y, HP, _, _, _), HP > 0,
    command_sequences(Last, _),
    next_sequence(Last, Seq),
    format(string(Command), 'атакуй бандита на ~d ~d id ~s номер ~d', [X, Y, UUID, Seq]).
building_command(repair(UUID), Command, repair_building(Seq, X, Y, UUID, _, _)) :-
    building(UUID, X, Y, _, _, _, _, _),
    command_sequences(_, Last),
    next_sequence(Last, Seq),
    format(string(Command), 'почини постройку на ~d ~d id ~s номер ~d', [X, Y, UUID, Seq]).

building_name(fence, "забор").
building_name(house, "дом").
building_name(watchtower, "башню").
building_name(sawmill, "лесопилку").
building_name(mine, "шахту").

next_sequence(Last, Seq) :-
    ( Last < 9223372036854775807 -> Seq is Last + 1
    ; throw(settlement_error(building_sequence_exhausted))
    ).

building_reply(Target, Expected, Reply, Excluded, NextExcluded, ReadyAt, NextReadyAt) :-
    ( Reply = ok(Expected) ->
        confirm_building(Expected),
        NextExcluded = Excluded,
        ( attack_target(Target) -> new_cooldown(NextReadyAt) ; NextReadyAt = ReadyAt )
    ; Reply = error(Code, _) ->
        log('действие ~q отклонено: ~s', [Target, Code]),
        ( attack_target(Target), Code == "attack-cooldown" ->
            new_cooldown(NextReadyAt), NextExcluded = Excluded
        ; memberchk(Code, ["bot-cooldown","stale-bandit","bandit-not-found","player-downed"]) ->
            NextReadyAt = ReadyAt, NextExcluded = Excluded
        ; NextReadyAt = ReadyAt,
          reject_target(retry_seconds, Code, Target, Excluded, NextExcluded)
        )
    ; throw(settlement_error(unexpected_building_reply))
    ).

attack_target(attack(_)).
attack_target(attack_bandit(_)).

new_cooldown(ReadyAt) :-
    get_time(Now),
    ReadyAt is Now + 1.

confirm_building(build(building(UUID, X, Y, Kind, _, _, _, _))) :-
    string(UUID), !,
    log('постройка подтверждена: ~s (~d, ~d), UUID ~s', [Kind, X, Y, UUID]).
confirm_building(attack_building(Seq, X, Y, _, HP, Destroyed)) :-
    log('атака №~d подтверждена: (~d, ~d), HP=~d, уничтожена=~w', [Seq, X, Y, HP, Destroyed]).
confirm_building(attack_bandit(Seq, X, Y, _, HP, Destroyed)) :-
    log('атака бандита ~d подтверждена: (~d, ~d), HP=~d, уничтожен=~w', [Seq, X, Y, HP, Destroyed]).
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
        ( Code == "bot-cooldown" -> wait_action(Session, 0.2), Outcome = cooldown
        ; gather_rejected(Code, Outcome)
        )
    ; throw(settlement_error(unexpected_gather_start_reply))
    ).

gather_progress(Session, Id, Target, Material, State, Progress, Outcome) :-
    ( nb_current(settlement_pacing, _), immediate_danger ->
        log('добыча прервана: бандиты рядом', []),
        Outcome = interrupted
    ; gathering_player(Id), Target = gather(Type, X, Y),
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
    ; Reply = error("bot-cooldown", _) ->
        wait_action(Session, 0.2),
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
        ( member(Change, Changes),
          memberchk(Change, ["building-destroyed","player-downed","player-respawned",
                             "bandit-killed","match-finished","match-started"]) ->
            log('событие v=~d: ~q resync=~w', [Version, Changes, Resync])
        ; true ),
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
