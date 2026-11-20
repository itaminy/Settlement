:- encoding(utf8).
:- use_module(settlement_dcg).
:- use_module(settlement_knowledge, except([resource/3])).

:- begin_tests(settlement_knowledge, [cleanup(reset_knowledge)]).

full_snapshot(Frame) :-
    atomics_to_string([
        "reply v=1 ok world-versioned version=10 width=20 height=10 self=1",
        " players=[player(1,2,3,blue,\"Анна\"),player(2,3,0,red,\"Борис\")]",
        " objects=[resource(0,3,tree),resource(2,1,tree),resource(3,3,stone),",
        "obstacle(4,3,house),",
        "building(6,2,01234567-89ab-cdef-0123-456789abcdef,fence,blue,1,40,50),",
        "building(7,2,11111111-2222-3333-4444-555555555555,house,red,2,190,200),",
        "building(9,2,none,fence,none,none,none,none)]",
        " total_wood=12 total_stone=18 blue=21 red=13 phase=active winner=none",
        " inventory_wood=3 inventory_stone=2 inventory_axe=1 inventory_pickaxe=0",
        " attack_seq=17 repair_seq=4 online=[1,2] gathering=[2]"
    ], Line),
    parse_frame(Line, ok(Frame)).

load_full :-
    full_snapshot(Frame),
    load_snapshot(Frame).

replacement(Version, reply(1, ok(world_versioned(Version, World, [1], [])))) :-
    World = world(size(20, 10), 1, [player(1, 5, 5, blue, "Анна")],
                  [resource(2, 1, tree),
                   building("01234567-89ab-cdef-0123-456789abcdef", 6, 2,
                            "fence", blue, 1, 50, 50)],
                  totals(3, 0), score(100, 13), match(finished, blue),
                  inventory(0, 7, 1, 1), sequences(18, 5)).

test(full_snapshot, [setup(reset_knowledge)]) :-
    load_full,
    assertion(world_size(20, 10)),
    assertion(world_version(10)),
    assertion(bot_identity(1, blue)),
    assertion(player(2, 3, 0, red, "Борис")),
    assertion(settlement_knowledge:resource(3, 3, stone)),
    assertion(building("01234567-89ab-cdef-0123-456789abcdef", 6, 2, "fence", blue, 1, 40, 50)),
    assertion(building(none, 9, 2, "fence", none, none, none, none)),
    assertion(obstacle(4, 3, "house")),
    assertion(world_totals(12, 18)),
    assertion(command_sequences(17, 4)),
    assertion(inventory_item(axe, 1)),
    assertion(inventory_item(pickaxe, 0)),
    assertion(online_player(1)),
    assertion(gathering_player(2)),
    assertion(\+ needs_resync).

test(replace_and_reload_without_duplicates, [setup(reset_knowledge)]) :-
    load_full,
    replacement(11, Frame),
    load_snapshot(Frame),
    load_snapshot(Frame),
    findall(Id, player(Id, _, _, _, _), Players),
    findall(X-Y-Type, settlement_knowledge:resource(X, Y, Type), Resources),
    findall(HP, building(_, _, _, _, _, _, HP, _), Durabilities),
    findall(Item-Amount, inventory_item(Item, Amount), Inventory),
    findall(Version, world_version(Version), Versions),
    assertion(Players == [1]),
    assertion(Resources == [2-1-tree]),
    assertion(Durabilities == [50]),
    assertion(Inventory == [wood-0, stone-7, axe-1, pickaxe-1]),
    assertion(Versions == [11]),
    assertion(\+ obstacle(_, _, _)),
    assertion(\+ online_player(2)),
    assertion(\+ gathering_player(_)),
    assertion(bot_position(5, 5)),
    assertion(current_score(100, 13)),
    assertion(match_state(finished, blue)).

bad_frame(reply(2, ok(world_versioned(11, invalid, [], [])))).
bad_frame(reply(1, ok(world_versioned(11, _, [], [])))).
bad_frame(reply(1, ok(world_versioned(11, invalid, [], [])))).
bad_frame(Frame) :-
    replacement(11, reply(1, ok(world_versioned(11, World, [1], [])))),
    World = world(Size, Self, Players, Objects, Totals, Score, Match, Inventory, Sequences),
    member(BadWorld, [
        world(Size, 99, Players, Objects, Totals, Score, Match, Inventory, Sequences),
        world(Size, Self, [player(1, 20, 5, blue, "Анна")], Objects, Totals, Score, Match, Inventory, Sequences),
        world(Size, Self, Players, [resource(2, 1, tree) | Objects], Totals, Score, Match, Inventory, Sequences),
        world(Size, Self, Players, [building("bad-uuid", 3, 1, "fence", blue, 1, 1, 2) | Objects],
              Totals, Score, Match, Inventory, Sequences),
        world(Size, Self, Players, Objects, Totals, Score, Match, inventory(0, -1, 1, 1), Sequences)
    ]),
    Frame = reply(1, ok(world_versioned(11, BadWorld, [1], []))).

test(invalid_snapshot_preserves_facts, [setup(reset_knowledge), forall(bad_frame(Frame))]) :-
    load_full,
    assertion(\+ load_snapshot(Frame)),
    assertion(world_version(10)),
    assertion(bot_position(2, 3)),
    assertion(settlement_knowledge:resource(3, 3, stone)),
    assertion(resource_counts(3, 2)),
    assertion(match_state(active, none)),
    assertion(\+ needs_resync).

test(transaction_rollback_after_replacement, [setup(reset_knowledge)]) :-
    load_full,
    replacement(11, Frame),
    catch(transaction((load_snapshot(Frame), world_version(11), bot_position(5, 5),
                       throw(rollback_test))),
          rollback_test, true),
    assertion(world_version(10)),
    assertion(bot_position(2, 3)),
    assertion(obstacle(4, 3, "house")),
    assertion(resource_counts(3, 2)).

test(consistent_reader_during_commit, [setup(reset_knowledge)]) :-
    load_full,
    replacement(11, Frame),
    snapshot((
        bot_position(2, 3),
        thread_create(load_snapshot(Frame), Writer, []),
        thread_join(Writer, true),
        world_version(10),
        bot_position(2, 3),
        settlement_knowledge:resource(3, 3, stone)
    )),
    assertion(world_version(11)),
    assertion(bot_position(5, 5)),
    assertion(\+ settlement_knowledge:resource(3, 3, stone)).

test(queries, [setup(reset_knowledge)]) :-
    load_full,
    assertion(bot_position(2, 3)),
    nearest_resource(Type, X, Y, Distance),
    assertion(Type-X-Y-Distance == stone-3-3-1),
    assertion(nearest_resource(tree, 0, 3, 2)),
    assertion(\+ nearest_resource(tree, 2, 1, 2)),
    findall(Id, allied_building(building(Id, _, _, _, _, _, _, _)), Allies),
    findall(Id, enemy_building(building(Id, _, _, _, _, _, _, _)), Enemies),
    assertion(Allies == ["01234567-89ab-cdef-0123-456789abcdef"]),
    assertion(Enemies == ["11111111-2222-3333-4444-555555555555"]),
    assertion(resource_counts(3, 2)),
    assertion(current_score(21, 13)),
    assertion(match_state(active, none)).

test(event_requires_snapshot_without_changing_world, [setup(reset_knowledge)]) :-
    load_full,
    parse_frame("event v=1 version=11 seq=11 changes=[resource-removed] resync=true", ok(Event)),
    apply_event(Event),
    apply_event(event(1, 12, 12, [], false)),
    assertion(needs_resync),
    assertion(world_version(10)),
    assertion(settlement_knowledge:resource(3, 3, stone)),
    replacement(11, Older),
    assertion(\+ load_snapshot(Older)),
    replacement(12, Fresh),
    load_snapshot(Fresh),
    assertion(\+ needs_resync),
    assertion(\+ settlement_knowledge:resource(3, 3, stone)).

test(covered_event_is_ignored_and_contiguous_event_advances_cursor, [setup(reset_knowledge)]) :-
    load_full,
    apply_event(event(1, 9, 9, [], true)),
    apply_event(event(1, 10, 10, [], true)),
    apply_event(event(1, 11, 11, [], false)),
    apply_event(event(1, 12, 12, [], false)),
    assertion(\+ needs_resync),
    assertion(world_version(10)).

test(sequence_violation, [setup(reset_knowledge), forall(member(Version-Seq, [13-13, 11-11, 13-12]))]) :-
    load_full,
    apply_event(event(1, 11, 11, [], false)),
    assertion(\+ needs_resync),
    apply_event(event(1, Version, Seq, [], false)),
    assertion(needs_resync),
    assertion(world_version(10)).

test(out_of_order_event, [setup(reset_knowledge)]) :-
    load_full,
    apply_event(event(1, 11, 11, [], false)),
    apply_event(event(1, 12, 12, [], false)),
    apply_event(event(1, 11, 11, [], false)),
    assertion(needs_resync).

test(event_before_first_snapshot, [setup(reset_knowledge)]) :-
    apply_event(event(1, 1, 1, [], false)),
    assertion(needs_resync),
    assertion(\+ world_version(_)).

test(session_reset_and_lower_version, [setup(reset_knowledge)]) :-
    load_full,
    apply_event(event(1, 11, 11, [], true)),
    reset_knowledge,
    assertion(needs_resync),
    assertion(\+ bot_identity(_, _)),
    assertion(\+ player(_, _, _, _, _)),
    assertion(\+ inventory_item(_, _)),
    assertion(\+ score(_, _)),
    assertion(\+ match_state(_, _)),
    assertion(\+ world_version(_)),
    assertion(\+ online_player(_)),
    assertion(\+ gathering_player(_)),
    replacement(1, Frame),
    load_snapshot(Frame),
    assertion(world_version(1)),
    assertion(\+ needs_resync),
    assertion(\+ player(2, _, _, _, _)),
    assertion(\+ nearest_resource(stone, _, _, _)).

test(unversioned_world, [setup(reset_knowledge)]) :-
    replacement(11, reply(1, ok(world_versioned(11, World, _, _)))),
    load_snapshot(reply(1, ok(World))),
    assertion(bot_position(5, 5)),
    assertion(\+ world_version(_)),
    assertion(needs_resync).

:- end_tests(settlement_knowledge).
