:- encoding(utf8).
:- begin_tests(settlement_strategy, [cleanup(reset_knowledge)]).

:- use_module(settlement_strategy).
:- use_module(settlement_bot, []).
:- use_module(settlement_client, [craft_tool/5]).
:- use_module(settlement_knowledge, except([resource/3])).

uuid("01234567-89ab-cdef-0123-456789abcdef").
enemy(HP, building(UUID, 4, 3, "fence", red, 2, HP, 50)) :- uuid(UUID).
ally(HP, building(UUID, 4, 3, "fence", blue, 1, HP, 50)) :- uuid(UUID).

load_world(Objects, Wood, Stone, Blue, Red, Match) :-
    reset_knowledge,
    World = world(size(8, 7), 1, [player(1, 3, 3, blue, "Bot")], Objects,
                  totals(0, 0), score(Blue, Red), Match,
                  inventory(Wood, Stone, 0, 0), sequences(5, 8)),
    load_snapshot(reply(1, ok(world_versioned(1, World, [1], [])))).

test(gathers_when_build_is_unaffordable) :-
    load_world([resource(3, 2, tree)], 1, 0, 0, 0, match(active, none)),
    choose_action([], 0, Action),
    assertion(Action == gather(tree, 3, 2)).

test(builds_without_walking_off_its_own_anchor) :-
    load_world([], 2, 0, 0, 0, match(active, none)),
    choose_action([], 0, Action),
    assertion(Action == build(fence, 2, 3)),
    choose_action([Action], 0, Next),
    assertion(Next == build(fence, 3, 2)).

test(funded_house_scores_more_per_command) :-
    load_world([resource(3, 2, tree)], 15, 10, 70, 0, match(active, none)),
    choose_action([], 0, Action),
    assertion(Action = build(house, _, _)).

test(uses_cheapest_winning_build_even_when_enemy_is_close) :-
    enemy(10, Enemy),
    load_world([Enemy], 15, 10, 98, 99, match(active, none)),
    choose_action([], 0, Action),
    assertion(Action = build(fence, _, _)).

test(crafts_axe_only_when_amortized) :-
    load_world([resource(3, 2, tree)], 5, 2, 0, 0, match(active, none)),
    choose_action([], 0, craft(axe)),
    load_world([resource(3, 2, tree)], 5, 2, 95, 0, match(active, none)),
    choose_action([], 0, Action),
    assertion(Action = build(fence, _, _)).

test(reserves_wood_to_prepare_an_axe) :-
    Resources = [resource(3, 2, tree), resource(3, 4, stone)],
    load_world(Resources, 2, 0, 0, 0, match(active, none)),
    choose_action([], 0, gather(tree, 3, 2)),
    load_world(Resources, 5, 0, 0, 0, match(active, none)),
    choose_action([], 0, gather(stone, 3, 4)).

test(reaches_enemy_then_observes_cooldown) :-
    uuid(UUID),
    Enemy = building(UUID, 5, 3, "fence", red, 2, 10, 50),
    load_world([Enemy], 0, 0, 0, 99, match(active, none)),
    choose_action([], 0, reposition(right, attack(UUID))),
    enemy(10, Adjacent),
    load_world([Adjacent], 0, 0, 0, 99, match(active, none)),
    choose_action([], 0.75, wait(0.75)),
    choose_action([], 0, attack(UUID)).

test(repairs_only_when_damaged_and_funded) :-
    ally(40, Damaged), uuid(UUID),
    load_world([Damaged], 1, 0, 2, 0, match(active, none)),
    choose_action([], 0, repair(UUID)),
    load_world([Damaged], 0, 0, 2, 0, match(active, none)),
    choose_action([], 0, stop(no_reachable_resource)),
    ally(50, Full),
    load_world([Full], 1, 0, 2, 0, match(active, none)),
    choose_action([], 0, stop(no_reachable_resource)).

test(unreachable_enemy_does_not_prevent_gathering) :-
    uuid(UUID),
    Enemy = building(UUID, 5, 3, "fence", red, 2, 10, 50),
    load_world([Enemy, obstacle(5, 2, "house"), obstacle(5, 4, "house"),
                obstacle(6, 3, "house"), obstacle(4, 3, "house"), resource(3, 2, tree)],
               0, 0, 0, 99, match(active, none)),
    choose_action([], 0, gather(tree, 3, 2)).

test(finished_match_distinguishes_victory_and_defeat) :-
    load_world([], 2, 0, 100, 90, match(finished, blue)),
    choose_action([], 0, stop(victory)),
    load_world([], 2, 0, 90, 100, match(finished, red)),
    choose_action([], 0, stop(defeat)).

snapshot_line(V, Objects, Wood, Blue, Red, Phase, Winner, Attack, Repair, Line) :-
    format(string(Line),
           'reply v=1 ok world-versioned version=~d width=8 height=7 self=1 players=[player(1,3,3,blue,"Bot")] objects=[~s] total_wood=0 total_stone=0 blue=~d red=~d phase=~w winner=~w inventory_wood=~d inventory_stone=0 inventory_axe=0 inventory_pickaxe=0 attack_seq=~d repair_seq=~d online=[1] gathering=[]',
           [V, Objects, Blue, Red, Phase, Winner, Wood, Attack, Repair]).

scripted(First, Replies, Output, Result) :-
    reset_knowledge,
    atomics_to_string([First, "reply v=1 ok subscribe-events since=1 version=1" | Replies],
                      "\n", Text),
    get_time(Now),
    Deadline is Now + 4,
    Counter = counter(0),
    setup_call_cleanup(
        open_string(Text, In),
        with_output_to(string(Output),
            ( current_output(Out),
              catch(settlement_bot:connected(1, 5, Deadline, fail, Counter, Result, In, Out),
                    settlement_error(disconnected), Result = disconnected)
            )),
        close(In)).

test(build_refusal_switches_anchor_then_reaches_100) :-
    snapshot_line(1, "", 2, 98, 90, active, none, 5, 8, First),
    uuid(UUID),
    format(string(Building), 'building(3,2,~s,fence,blue,1,50,50)', [UUID]),
    format(string(Confirmation), 'reply v=1 ok build building=~s', [Building]),
    snapshot_line(2, Building, 0, 100, 90, finished, blue, 5, 8, Won),
    scripted(First, ["reply v=1 error code=protected-area", First, Confirmation, Won],
             Output, Result),
    assertion(Result == victory),
    findall(Entry, (member(Entry, ["построй забор на 2 3\n", "построй забор на 3 2\n"]),
                    sub_string(Output, _, _, _, Entry)), Commands),
    assertion(Commands == ["построй забор на 2 3\n", "построй забор на 3 2\n"]),
    assertion(current_score(100, 90)).

test(connection_loss_does_not_replay_a_build) :-
    snapshot_line(1, "", 2, 0, 0, active, none, 5, 8, First),
    scripted(First, [], Output, Result),
    assertion(Result == disconnected),
    findall(Start, sub_string(Output, Start, _, _, "построй забор"), Starts),
    assertion(Starts = [_]).

test(attack_uses_snapshot_sequence_and_stops_on_destruction) :-
    uuid(UUID),
    format(string(Building), 'building(4,3,~s,fence,red,2,10,50)', [UUID]),
    snapshot_line(1, Building, 0, 0, 2, active, none, 5, 8, First),
    format(string(Attack),
           'reply v=1 ok attack-building attack_seq=6 x=4 y=3 building_id=~s hp=0 destroyed=true',
           [UUID]),
    snapshot_line(2, "", 0, 0, 0, active, none, 6, 8, After),
    scripted(First, [Attack, After], Output, Result),
    assertion(Result == no_reachable_resource),
    format(string(Command), 'атакуй постройку на 4 3 id ~s номер 6~n', [UUID]),
    findall(Start, sub_string(Output, Start, _, _, Command), Starts),
    assertion(Starts = [_]),
    assertion(command_sequences(6, 8)).

test(cooldown_response_does_not_trigger_an_immediate_retry) :-
    uuid(UUID),
    format(string(Building), 'building(4,3,~s,fence,red,2,10,50)', [UUID]),
    snapshot_line(1, Building, 0, 0, 2, active, none, 5, 8, First),
    snapshot_line(1, Building, 0, 0, 2, active, none, 6, 8, After),
    snapshot_line(2, Building, 0, 0, 100, finished, red, 6, 8, Lost),
    get_time(Before),
    scripted(First, ["reply v=1 error code=attack-cooldown", After, Lost], Output, Result),
    get_time(AfterWait),
    assertion(Result == defeat),
    assertion(AfterWait - Before >= 0.18),
    findall(Start, sub_string(Output, Start, _, _, "атакуй постройку"), Starts),
    assertion(Starts = [_]).

test(repair_uses_snapshot_sequence_and_updates_hp) :-
    uuid(UUID),
    format(string(Damaged), 'building(4,3,~s,fence,blue,1,47,50)', [UUID]),
    format(string(Full), 'building(4,3,~s,fence,blue,1,50,50)', [UUID]),
    snapshot_line(1, Damaged, 1, 2, 0, active, none, 5, 8, First),
    format(string(Repair),
           'reply v=1 ok repair-building repair_seq=9 x=4 y=3 building_id=~s hp=50 restored_hp=3',
           [UUID]),
    snapshot_line(2, Full, 0, 2, 0, active, none, 5, 9, After),
    scripted(First, [Repair, After], Output, Result),
    assertion(Result == no_reachable_resource),
    format(string(Command), 'почини постройку на 4 3 id ~s номер 9~n', [UUID]),
    assertion(sub_string(Output, _, _, _, Command)),
    assertion(building(UUID, 4, 3, "fence", blue, 1, 50, 50)),
    assertion(command_sequences(5, 9)).

test(craft_uses_edn_and_handles_status, [forall(member(Line-Expected, [
        "{:status :ok, :world {:players {1 {:name \"Bot\"}}}}"-ok,
        "{:status :error, :code :insufficient-items, :world {}}"-error("insufficient-items")
    ]))]) :-
    setup_call_cleanup(
        open_string(Line, In),
        with_output_to(string(Command),
            (current_output(Out), craft_tool(1, axe, In, Out, Result))),
        close(In)),
    assertion(Command == "{:type :craft :player-id 1 :tool :axe}\n"),
    assertion(Result == Expected).

:- end_tests(settlement_strategy).
