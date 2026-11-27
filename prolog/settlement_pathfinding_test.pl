:- begin_tests(settlement_pathfinding, [cleanup(reset_knowledge)]).

:- use_module(settlement_knowledge, [load_snapshot/1, reset_knowledge/0, known_resource/3]).
:- use_module(settlement_pathfinding).

load_map(Width, Height, X, Y, Others, Objects) :-
    reset_knowledge,
    update_map(1, Width, Height, X, Y, Others, Objects).

update_map(Version, Width, Height, X, Y, Others, Objects) :-
    World = world(size(Width, Height), 1, [player(1, X, Y, blue, "Bot") | Others],
                  Objects, totals(0, 0), score(0, 0), match(active, none),
                  inventory(0, 0, 0, 0), sequences(0, 0)),
    load_snapshot(reply(1, ok(world_versioned(Version, World, [1], [])))).

test(direction_and_full_route) :-
    load_map(5, 5, 2, 2, [], []),
    path_to(cell(2, 0), [up, up]),
    path_to(cell(2, 4), [down, down]),
    path_to(cell(0, 2), [left, left]),
    path_to(cell(4, 2), [right, right]),
    path_to(cell(2, 2), []).

test(unique_detour_around_objects_and_player) :-
    load_map(5, 3, 0, 0, [player(2, 1, 0, red, "Other")],
             [resource(2, 0, tree), resource(3, 0, stone), obstacle(2, 1, "house"),
              building(none, 0, 2, "fence", none, none, none, none)]),
    path_to(cell(4, 0), Route),
    assertion(Route == [down, right, down, right, right, right, up, up]).

test(static_house_blocks_only_its_recorded_cell) :-
    load_map(4, 2, 0, 1, [], [obstacle(1, 0, "house")]),
    path_to(cell(2, 1), [right, right]).

test(rectangular_structures_occupy_the_same_cells_as_the_server) :-
    ID = "01234567-89ab-cdef-0123-456789abcdef",
    forall(member(Kind, ["watchtower", "sawmill", "mine"]),
           ( load_map(8, 8, 0, 0, [], [building(ID, 3, 3, Kind, red, 2, 100, 100)]),
             path_to(cell(4, 3), no_path),
             path_to(cell(4, 4), Route),
             ( Kind == "sawmill" -> assertion(Route == no_path) ; assertion(is_list(Route)) ) )).

test(live_bandits_block_paths_and_waiting_bandits_do_not) :-
    Id = "01234567-89ab-cdef-0123-456789abcdef",
    load_map(3, 1, 0, 0, [], [bandit(Id, 1, 0, 100, 100, "patrol", "right")]),
    path_to(cell(2,0), no_path),
    path_to(attack_bandit(Id), []),
    update_map(2, 3, 1, 0, 0, [], [bandit(Id, none, none, 0, 100, "waiting", "right")]),
    path_to(cell(2,0), [right,right]),
    path_to(attack_bandit(Id), no_path).

test(large_world_crosses_only_at_reported_bridge) :-
    findall(obstacle(50, Y, "water"), (between(0, 99, Y), Y =\= 51), River),
    load_map(100, 100, 49, 49, [], [obstacle(52, 49, "mountain") | River]),
    path_to(cell(51, 49), [down, down, right, right, up, up]),
    path_to(cell(50, 49), no_path),
    path_to(cell(52, 49), no_path),
    path_to(cell(100, 51), no_path).

test(no_path_and_invalid_targets) :-
    load_map(3, 3, 0, 1, [],
             [resource(1, 0, tree), obstacle(1, 1, "house"), resource(1, 2, stone)]),
    path_to(cell(2, 1), no_path),
    path_to(cell(-1, 1), no_path),
    path_to(cell(0, 3), no_path),
    path_to(cell(1, 1), no_path),
    path_to(gather(stone, 1, 0), no_path).

test(gather_uses_cardinal_adjacency) :-
    load_map(5, 4, 1, 1, [], [resource(2, 2, tree), resource(1, 0, stone)]),
    path_to(gather(tree, 2, 2), Route),
    assertion(memberchk(Route, [[right], [down]])),
    path_to(gather(stone, 1, 0), []).

test(gather_uses_reachable_side) :-
    load_map(5, 5, 0, 2, [],
             [resource(2, 2, tree), obstacle(1, 2, "house"),
              obstacle(2, 1, "house"), obstacle(2, 3, "house")]),
    path_to(gather(tree, 2, 2), Route),
    assertion(length(Route, 7)).

test(attack_from_far_side_of_house) :-
    UUID = "01234567-89ab-cdef-0123-456789abcdef",
    load_map(6, 5, 5, 2, [],
             [building(UUID, 2, 1, "house", red, 2, 190, 200)]),
    path_to(attack(UUID), [left]),
    path_to(repair(UUID), no_path),
    forall(member(X-Y, [2-1, 2-2, 3-1, 3-2]), path_to(cell(X, Y), no_path)).

test(diagonal_to_house_is_not_adjacent) :-
    UUID = "01234567-89ab-cdef-0123-456789abcdef",
    load_map(6, 5, 4, 3, [],
             [building(UUID, 2, 1, "house", blue, 1, 190, 200)]),
    path_to(repair(UUID), Route),
    assertion(memberchk(Route, [[left], [up]])),
    path_to(attack(UUID), no_path).

test(build_within_two_steps_of_footprint) :-
    load_map(8, 5, 7, 2, [], []),
    path_to(build(house, 3, 1), [left]),
    path_to(build(fence, 5, 2), []),
    path_to(build(house, 7, 2), no_path).

test(build_plot_blocked_by_objects_or_other_players) :-
    load_map(7, 5, 1, 2, [player(2, 4, 3, red, "Other")],
             [resource(2, 0, stone)]),
    path_to(build(house, 3, 2), no_path),
    path_to(build(house, 1, 0), no_path),
    path_to(build(fence, 1, 2), Route),
    assertion(length(Route, 1)).

test(no_snapshot) :-
    reset_knowledge,
    path_to(cell(0, 0), no_path).

test(future_resource_restore_preserves_its_last_access) :-
    Obstacles = [obstacle(3, 1, "house"), obstacle(4, 2, "house"), obstacle(3, 3, "house")],
    load_map(6, 6, 1, 2, [], [resource(3, 2, tree) | Obstacles]),
    update_map(2, 6, 6, 1, 2, [], Obstacles),
    assertion(known_resource(3, 2, tree)),
    construction_terrain(Terrain),
    assertion(\+ preserves_access(Terrain, build(fence, 2, 2))),
    assertion(\+ preserves_access(Terrain, build(fence, 3, 2))),
    assertion(preserves_access(Terrain, build(fence, 1, 1))),
    reset_knowledge,
    assertion(\+ known_resource(_, _, _)).

test(building_may_not_sever_a_corridor) :-
    load_map(5, 3, 1, 1, [], [obstacle(2, 0, "house"), obstacle(2, 2, "house")]),
    construction_terrain(Terrain),
    assertion(\+ preserves_access(Terrain, build(fence, 2, 1))).

test(protected_road_is_walkable_but_cannot_be_built_on) :-
    load_map(5, 3, 1, 1, [], [reserved(2, 1)]),
    path_to(cell(3, 1), [right, right]),
    construction_terrain(Terrain),
    assertion(\+ preserves_access(Terrain, build(house, 2, 1))),
    assertion(preserves_access(Terrain, build(fence, 3, 1))).

:- end_tests(settlement_pathfinding).
