:- module(settlement_strategy, [choose_action/3, choose_resource/3]).

:- use_module(settlement_knowledge, except([resource/3])).
:- use_module(settlement_pathfinding).

% choose_action(+Excluded, +AttackCooldownSeconds, -Action)
% Exclusions identify targets rejected during this run. A routed action with
% remaining steps produces reposition(Direction, Target).
choose_action(Excluded, Cooldown, Action) :-
    snapshot(decide(Excluded, Cooldown, Action)).

decide(Excluded, Cooldown, Action) :-
    ( match_state(finished, Winner), bot_identity(_, Team) ->
        ( Winner == Team -> Result = victory ; Result = defeat ),
        Action = stop(Result)
    ; match_state(active, _), bot_identity(_, Team),
      opposing_team(Team, Enemy),
      score(Team, Ours), score(Enemy, Theirs),
      resource_counts(Wood, Stone) ->
        Need is 100 - Ours,
        position_plan(Excluded, Cooldown, Need, Theirs, Wood, Stone, Action)
    ; Action = stop(no_world)
    ).

position_plan(Excluded, Cooldown, Need, Theirs, Wood, Stone, Action) :-
    ( build_plan(winning, Need, Wood, Stone, Excluded, Target, Route) ->
        routed_action(Route, Target, Action)
    ; repair_plan(critical, Wood, Excluded, Target, Route) ->
        routed_action(Route, Target, Action)
    ; Theirs >= 85, attack_plan(urgent, Excluded, Target, Route) ->
        attack_action(Route, Target, Cooldown, Action)
    ; axe_plan(Need, Wood, Stone, Excluded, Action) ->
        true
    ; build_plan(regular, Need, Wood, Stone, Excluded, Target, Route) ->
        routed_action(Route, Target, Action)
    ; repair_plan(regular, Wood, Excluded, Target, Route) ->
        routed_action(Route, Target, Action)
    ; attack_plan(local, Excluded, Target, Route) ->
        attack_action(Route, Target, Cooldown, Action)
    ; (Wood < 15 ; Stone < 10), gather_plan(Need, Wood, Stone, Excluded, Target, Route) ->
        routed_action(Route, Target, Action)
    ; attack_plan(urgent, Excluded, Target, Route) ->
        attack_action(Route, Target, Cooldown, Action)
    ; Action = stop(no_reachable_resource)
    ).

opposing_team(blue, red).
opposing_team(red, blue).

available(Excluded, Target) :- \+ memberchk(Target, Excluded).

routed_action([], Target, Target).
routed_action([Direction | _], Target, reposition(Direction, Target)).

attack_action([], _, Cooldown, wait(Cooldown)) :- Cooldown > 0, !.
attack_action(Route, Target, _, Action) :- routed_action(Route, Target, Action).

% world.clj scores and buildings.clj catalog.
building_spec(fence, 2, 0, 2).
building_spec(house, 15, 10, 15).

% Prefer a fence when it closes the match. A funded house with a larger
% gap scores in one command and uses no more wood per point than fences.
build_plan(Mode, Need, Wood, Stone, Excluded, Target, Route) :-
    build_order(Need, Kinds),
    member(Kind, Kinds),
    building_spec(Kind, CostWood, CostStone, Points),
    Wood >= CostWood, Stone >= CostStone,
    ( Mode == winning -> Need =< Points ; true ),
    build_location(Kind, Excluded, Target, Route),
    !.

build_order(Need, [fence, house]) :- Need =< 2, !.
build_order(_, [house, fence]).

build_location(Kind, Excluded, Target, Route) :-
    bot_position(BX, BY),
    world_size(Width, Height),
    construction_terrain(Terrain),
    ( Kind == house -> Offset = 1 ; Offset = 0 ),
    MaxX is Width - 1 - Offset, MaxY is Height - 1 - Offset,
    findall(key(Lower, Distance, X, Y)-Candidate,
            ( between(0, MaxX, X), between(0, MaxY, Y),
              Candidate = build(Kind, X, Y), available(Excluded, Candidate),
              Distance is max(0, max(X - BX, BX - X - Offset)) +
                            max(0, max(Y - BY, BY - Y - Offset)),
              ( Distance =:= 0 -> Lower = 1 ; Lower is max(0, Distance - 2) )
            ), Unsorted),
    keysort(Unsorted, Candidates),
    shortest_build(Candidates, Terrain, none, best(_, Target, Route)).

shortest_build([], _, Best, Best).
shortest_build([key(Lower, _, _, _)-Target | Rest], Terrain, Best, Result) :-
    ( Best = best(Length, _, _), Lower >= Length ->
        Result = Best
    ; ( preserves_access(Terrain, Target),
        path_to(Target, Route), is_list(Route), length(Route, Steps),
        ( Best == none ; Best = best(OldLength, _, _), Steps < OldLength ) ->
          Next = best(Steps, Target, Route)
      ; Next = Best
      ),
      shortest_build(Rest, Terrain, Next, Result)
    ).

% A new axe costs 3 wood and 2 stone and saves 2 seconds per future tree.
% Count the gathering time of its ingredients and keep one fence funded.
axe_plan(Need, Wood, Stone, Excluded, Action) :-
    inventory_item(axe, 0), available(Excluded, craft(axe)),
    ( Wood < 15 ; Stone < 10 ),
    Trees is max(0, ((Need + 1) // 2) * 2 - Wood),
    ( inventory_item(pickaxe, 0) -> StoneMs = 4000 ; StoneMs = 1500 ),
    Trees * 2000 > 3 * 3000 + 2 * StoneMs,
    choose_resource_type(tree, Excluded, Tree, TreeRoute),
    ( Stone < 2 ->
        choose_resource_type(stone, Excluded, Rock, RockRoute),
        ( Wood < 5 -> routed_action(TreeRoute, Tree, Action)
        ; routed_action(RockRoute, Rock, Action)
        )
    ; Wood < 5 ->
        routed_action(TreeRoute, Tree, Action)
    ; Action = craft(axe)
    ).

repair_plan(Mode, Wood, Excluded, Target, Route) :-
    Wood >= 1,
    findall(key(Urgency, Steps, UUID)-(Candidate-Path),
            ( allied_building(building(UUID, _, _, _, _, _, HP, MaxHP)),
              string(UUID), integer(HP), integer(MaxHP), HP < MaxHP,
              ( Mode == critical -> HP =< 10 ; true ),
              Candidate = repair(UUID), available(Excluded, Candidate),
              path_to(Candidate, Path), is_list(Path), length(Path, Steps),
              Urgency is HP
            ), Candidates),
    keysort(Candidates, [_-(Target-Route) | _]).

attack_plan(Mode, Excluded, Target, Route) :-
    findall(key(Effort, NegPoints, Steps, UUID)-(Candidate-Path),
            ( enemy_building(building(UUID, _, _, Kind, _, _, HP, _)),
              string(UUID), integer(HP), HP > 0,
              atom_string(Type, Kind), building_spec(Type, _, _, Points),
              Candidate = attack(UUID), available(Excluded, Candidate),
              path_to(Candidate, Path), is_list(Path), length(Path, Steps),
              Hits is (HP + 9) // 10, Effort is Steps + Hits,
              ( Mode == local -> Effort =< 8 ; true ),
              NegPoints is -Points
            ), Candidates),
    keysort(Candidates, [_-(Target-Route) | _]).

gather_plan(Need, Wood, Stone, Excluded, Target, Route) :-
    % Late in the match, fill a small gap for a house that can win at once.
    ( Need > 2, Need =< 15, Wood >= 15, Stone >= 8, Stone < 10,
      choose_resource_type(stone, Excluded, Target, Route) -> true
    ; choose_resource(Excluded, Target, Route)
    ).

choose_resource(Excluded, Target, Route) :-
    snapshot(choose_resource_type(_, Excluded, Target, Route)).

% Manhattan distance minus one only prunes candidates that cannot match
% the best route found by A*.
choose_resource_type(Preferred, Excluded, Target, Route) :-
    bot_position(BX, BY),
    findall(key(Lower, Rank, X, Y)-Candidate,
            ( settlement_knowledge:resource(X, Y, Type),
              ( var(Preferred) -> true ; Preferred == Type ),
              Candidate = gather(Type, X, Y), available(Excluded, Candidate),
              resource_rank(Type, Rank),
              Lower is max(0, abs(X - BX) + abs(Y - BY) - 1)
            ), Candidates),
    keysort(Candidates, Sorted),
    shortest_resource(Sorted, none, best(_, Target, Route)).

resource_rank(tree, 0).
resource_rank(stone, 1).

shortest_resource([], Best, Best).
shortest_resource([key(Lower, Rank, X, Y)-Target | Rest], Best, Result) :-
    ( Best = best(key(Length, _, _, _), _, _), Lower > Length ->
        Result = Best
    ; ( path_to(Target, Route), is_list(Route), length(Route, Steps),
        Key = key(Steps, Rank, X, Y),
        ( Best == none ; Best = best(OldKey, _, _), Key @< OldKey ) ->
          Next = best(Key, Target, Route)
      ; Next = Best
      ),
      shortest_resource(Rest, Next, Result)
    ).
