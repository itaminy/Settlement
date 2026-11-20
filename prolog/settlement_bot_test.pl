:- encoding(utf8).
:- begin_tests(settlement_bot, [cleanup(reset_knowledge)]).

:- use_module(settlement_bot).
:- use_module(settlement_knowledge, except([resource/3])).
:- use_module(library(memfile)).

load_map(Objects) :-
    reset_knowledge,
    World = world(size(8, 6), 1, [player(1, 0, 2, blue, "Bot")], Objects,
                  totals(0, 0), score(0, 0), match(active, none),
                  inventory(0, 0, 0, 0), sequences(0, 0)),
    load_snapshot(reply(1, ok(world_versioned(1, World, [1], [])))).

test(shortest_reachable_uses_astar_length) :-
    load_map([resource(2, 2, tree), obstacle(1, 2, "house"),
              obstacle(2, 1, "house"), obstacle(2, 3, "house"),
              resource(0, 5, stone)]),
    choose_resource([], Target, Route),
    assertion(Target-Route == gather(stone, 0, 5)-[down, down]).

test(unreachable_and_excluded_targets_are_skipped) :-
    load_map([resource(2, 2, tree), obstacle(1, 2, "house"),
              obstacle(2, 1, "house"), obstacle(2, 3, "house"),
              obstacle(3, 2, "house"), resource(0, 5, stone)]),
    choose_resource([], Target, _),
    assertion(Target == gather(stone, 0, 5)),
    assertion(\+ choose_resource([Target], _, _)).

test(prefers_tree_at_equal_path_length) :-
    load_map([resource(0, 0, stone), resource(0, 4, tree)]),
    choose_resource([], Target, Route),
    assertion(Target-Route == gather(tree, 0, 4)-[down]).

snapshot_line(V, X, Y, Objects, Wood, Gathering, Line) :-
    format(string(Line),
           'reply v=1 ok world-versioned version=~d width=8 height=6 self=1 players=[player(1,~d,~d,blue,"Bot")] objects=[~s] total_wood=~d total_stone=0 blue=0 red=0 phase=active winner=none inventory_wood=~d inventory_stone=0 inventory_axe=0 inventory_pickaxe=0 attack_seq=0 repair_seq=0 online=[1] gathering=[~s]',
           [V, X, Y, Objects, Wood, Wood, Gathering]).

subscribe_reply("reply v=1 ok subscribe-events since=1 version=1").

:- meta_predicate scripted(+, 2, -).
scripted(Frames, Goal, Commands) :-
    reset_knowledge,
    atomics_to_string(Frames, "\n", Input),
    setup_call_cleanup(
        new_memory_file(Memory),
        ( setup_call_cleanup(
              open_string(Input, In),
              setup_call_cleanup(
                  open_memory_file(Memory, write, Out, [encoding(utf8)]),
                  once(call(Goal, In, Out)),
                  close(Out)),
              close(In)),
          memory_file_to_string(Memory, Text, utf8),
          split_string(Text, "\n", "\r", Lines),
          once(append(Commands, [""], Lines))
        ),
        free_memory_file(Memory)).

run_script(Max, Seconds, Result, In, Out) :-
    get_time(Now),
    Deadline is Now + Seconds,
    Counter = counter(0),
    with_output_to(string(_),
        settlement_bot:connected(1, Max, Deadline, fail, Counter, Reason, In, Out)),
    arg(1, Counter, Count),
    Result = stopped(Reason, Count).

test(move_refusal_refreshes_and_routes_around_new_obstacle) :-
    snapshot_line(1, 0, 2, "resource(3,2,tree)", 0, "", First),
    Obstructed = "obstacle(0,3,house),obstacle(1,2,house),resource(3,2,tree)",
    snapshot_line(2, 0, 2, Obstructed, 0, "", Blocked),
    snapshot_line(3, 0, 1, Obstructed, 0, "", Moved),
    subscribe_reply(Subscribed),
    scripted([First, Subscribed,
              "event v=1 version=2 seq=2 changes=[building-created] resync=true",
              "reply v=1 error code=object-blocked", Blocked,
              "reply v=1 ok move x=0 y=1", Moved],
             run_script(2, 3, Result), Commands),
    assertion(Result == stopped(action_limit, 2)),
    assertion(Commands == ["покажи мир с версией", "подпишись на события",
                           "иди вправо", "покажи мир с версией",
                           "иди вверх", "покажи мир с версией"]),
    assertion(world_version(3)),
    assertion(bot_position(0, 1)),
    assertion(\+ needs_resync).

test(no_resource_finishes_without_actions) :-
    snapshot_line(1, 0, 2, "", 0, "", Frame),
    subscribe_reply(Subscribed),
    scripted([Frame, Subscribed], run_script(100, 3, Result), Commands),
    assertion(Result == stopped(no_reachable_resource, 0)),
    assertion(Commands == ["покажи мир с версией", "подпишись на события"]).

test(gather_refusal_blacklists_target_and_tries_the_other_one) :-
    snapshot_line(1, 0, 2, "resource(0,1,tree),resource(0,3,stone)", 0, "", Frame),
    subscribe_reply(Subscribed),
    Refusal = "reply v=1 error code=no-matching-resource",
    scripted([Frame, Subscribed, Refusal, Frame, Refusal, Frame],
             run_script(100, 3, Result), Commands),
    assertion(Result == stopped(no_reachable_resource, 2)),
    Commands = [_, _, Tree, "покажи мир с версией", Stone, "покажи мир с версией"],
    assertion(sub_string(Tree, 0, _, _, "собери дерево на 0 1 попытка ")),
    assertion(sub_string(Stone, 0, _, _, "собери камень на 0 3 попытка ")),
    assertion(resource_counts(0, 0)).

attempt("01234567-89ab-4def-8123-456789abcdef").
operation("11111111-2222-4333-8444-555555555555").

progress_line(Kind, Duration, Elapsed, Line) :-
    attempt(Attempt), operation(Operation),
    format(string(Line),
           'reply v=1 ok ~s operation_id=~s attempt_id=~s duration_ms=~d elapsed_ms=~d',
           [Kind, Operation, Attempt, Duration, Elapsed]).

gather_script(Stop, Result, In, Out) :-
    get_time(Now),
    Deadline is Now + 3,
    attempt(Attempt),
    Session = session(In, Out, Deadline, plunit_settlement_bot:Stop, 10, counter(0), ready),
    with_output_to(string(_),
        catch(setup_call_cleanup(
                  State = attempt(Attempt, armed),
                  settlement_bot:start_gather(Session, 1, gather(tree, 0, 1), State, Result),
                  settlement_bot:cancel_if_armed(Session, State)),
              bot_stop(Reason), Result = stopped(Reason))).

test(gather_heartbeat_push_resync_and_confirmed_inventory) :-
    progress_line("gather-start", 401, 0, Start),
    progress_line("gather-heartbeat", 401, 401, Heartbeat),
    snapshot_line(2, 0, 2, "resource(0,1,tree)", 0, "1", Gathering),
    snapshot_line(3, 0, 2, "resource(0,1,tree)", 0, "1", Resynced),
    operation(Operation),
    format(string(Finish), 'reply v=1 ok gather-finish operation_id=~s resource=wood amount=1 x=0 y=1',
           [Operation]),
    snapshot_line(4, 0, 2, "", 1, "", Inventory),
    scripted([Start, Gathering, "event v=1 version=3 seq=3 changes=[player-moved] resync=true",
              Heartbeat, Resynced, Finish, Inventory], finish_script, Commands),
    Commands = [_, "покажи мир с версией", Continue, "покажи мир с версией",
                Complete, "покажи мир с версией"],
    assertion(sub_string(Continue, 0, _, _, "продолжай добычу ")),
    assertion(sub_string(Complete, 0, _, _, "заверши добычу ")),
    assertion(world_version(4)),
    assertion(resource_counts(1, 0)).

finish_script(In, Out) :-
    gather_script(fail, gathered, In, Out),
    assertion(world_version(3)),
    assertion(resource_counts(0, 0)),
    get_time(Now),
    Deadline is Now + 3,
    Session = session(In, Out, Deadline, fail, 10, counter(0), ready),
    with_output_to(string(_), settlement_bot:refresh(Session, 1)).

test(requested_stop_cancels_active_gather) :-
    progress_line("gather-start", 3000, 0, Start),
    snapshot_line(2, 0, 2, "resource(0,1,tree)", 0, "1", Gathering),
    attempt(Attempt),
    format(string(Cancelled), 'reply v=1 ok gather-cancel operation_id=none attempt_id=~s', [Attempt]),
    scripted([Start, Gathering, Cancelled], gather_script(gathering_player(1), Result), Commands),
    assertion(Result == stopped(requested)),
    format(string(Cancel), 'отмени попытку добычи ~s', [Attempt]),
    assertion(Commands = [_, "покажи мир с версией", Cancel]),
    assertion(resource_counts(0, 0)).

test(too_early_finish_renews_heartbeat_and_retries) :-
    progress_line("gather-start", 20, 20, Start),
    snapshot_line(2, 0, 2, "resource(0,1,tree)", 0, "1", Gathering),
    attempt(Attempt), operation(Operation),
    format(string(Early),
           'reply v=1 error code=too-early operation_id=~s attempt_id=~s duration_ms=20 elapsed_ms=19',
           [Operation, Attempt]),
    progress_line("gather-heartbeat", 20, 20, Heartbeat),
    format(string(Finish), 'reply v=1 ok gather-finish operation_id=~s resource=wood amount=1 x=0 y=1',
           [Operation]),
    scripted([Start, Gathering, Early, Heartbeat, Finish],
             gather_script(fail, gathered), Commands),
    format(string(Complete), 'заверши добычу ~s', [Operation]),
    format(string(Continue), 'продолжай добычу ~s', [Operation]),
    assertion(Commands = [_, "покажи мир с версией", Complete, Continue, Complete]),
    assertion(resource_counts(0, 0)).

test(action_limit_zero) :-
    scripted([], run_script(0, 3, Result), Commands),
    assertion(Result == stopped(action_limit, 0)),
    assertion(Commands == []).

test(elapsed_deadline) :-
    scripted([], run_script(100, -1, Result), Commands),
    assertion(Result == stopped(time_limit, 0)),
    assertion(Commands == []).

:- end_tests(settlement_bot).
