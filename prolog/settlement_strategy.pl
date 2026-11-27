:- module(settlement_strategy, [choose_action/3, choose_decision/5, choose_resource/3,
                                 immediate_danger/0]).

:- use_module(settlement_knowledge, except([resource/3])).
:- use_module(settlement_pathfinding).
:- use_module(settlement_strategy_config).

choose_action(Excluded, Cooldown, Action) :-
    choose_decision(Excluded, Cooldown, none, decision(_, _, _, Action), _).

% The decision and all route estimates use the same version of the facts.
choose_decision(Excluded, Cooldown, Previous, Decision, Unreachable) :-
    snapshot(decide(Excluded, Cooldown, Previous, Decision, Unreachable)).

decide(_, _, _, decision(none, match_finished, 0, stop(Result)), []) :-
    match_state(finished, Winner), bot_identity(_, Team), !,
    ( Winner == Team -> Result = victory ; Result = defeat ).
decide(_, _, _, decision(none, respawn, 0, wait(1)), []) :-
    match_state(active, _), bot_health(0), !.
decide(Excluded, Cooldown, Previous, Decision, Unreachable) :-
    match_state(active, _), bot_identity(_, Team),
    opposing_team(Team, Enemy), score(Team, Ours), score(Enemy, Theirs),
    resource_counts(Wood, Stone), bot_health(HP), !,
    priority(victory_score, Victory),
    Need is Victory - Ours,
    Context = situation(Need, Theirs, Wood, Stone, HP, Excluded),
    findall(Result,
            ( proposal(Context, Result)
            ; continue_retreat(HP, Previous, Excluded, Result)
            ), Proposals),
    findall(Target, member(unreachable(Target), Proposals), Unreachable),
    findall(Key-candidate(Target, Reason, Utility, Route),
            ( member(candidate(Target, Reason, Base, Route), Proposals),
              utility(Target, Previous, Route, Base, Utility),
              Key is -Utility ),
            Ranked),
    ( keysort(Ranked, [_-candidate(Target, Reason, Utility, Route)|_]) ->
        ( combat_target(Target) -> attack_action(Route, Target, Cooldown, Action)
        ; routed_action(Route, Target, Action)
        ),
        Decision = decision(Target, Reason, Utility, Action)
    ; priority(critical_hp, Critical), HP =< Critical ->
        Decision = decision(none, seek_safe_recovery, 0, wait(1))
    ; Decision = decision(none, no_reachable_goal, 0, stop(no_reachable_resource))
    ).
decide(_, _, _, decision(none, no_world, 0, stop(no_world)), []).

utility(Target, Previous, Route, Base, Utility) :-
    length(Route, Steps),
    route_exposure(Route, Exposure),
    priority(route_step, StepCost),
    priority(route_danger, DangerCost),
    ( Target == Previous -> priority(commitment, Bonus) ; Bonus = 0 ),
    Utility is round(Base - Steps*StepCost - Exposure*DangerCost + Bonus).

route_exposure(Route, Exposure) :-
    bot_position(X, Y),
    route_exposure(Route, X, Y, 0, Exposure).
route_exposure([], _, _, Exposure, Exposure).
route_exposure([Direction|Rest], X, Y, Before, Exposure) :-
    settlement_pathfinding:delta(Direction, DX, DY),
    NX is X+DX, NY is Y+DY,
    ( once((live_bandit(_, BX, BY, _, _, _), distance(NX, NY, BX, BY, D), D =< 2)) ->
        Next is Before+1
    ; Next = Before ),
    route_exposure(Rest, NX, NY, Next, Exposure).

proposal(Context, Candidate) :- development(Context, Candidate).
proposal(Context, Candidate) :- repair_proposal(Context, Candidate).
proposal(Context, Candidate) :- raid(Context, Candidate).
proposal(Context, Candidate) :- defence(Context, Candidate).

development(situation(Need, _, Wood, Stone, _, Excluded), Candidate) :-
    settlement_kind(Kind),
    building_spec(Kind, CostWood, CostStone, Points),
    Wood >= CostWood, Stone >= CostStone,
    priority(build, Base), priority(build_cost, CostWeight), priority(build_points, PointWeight),
    atom_string(Kind, Name),
    ( owned_structure(Name, _, _) ->
        ( Points < Need, missing_kind(_) ->
            priority(repeated_building_kind, Penalty), Diversity is -Penalty
        ; Diversity = 0 )
    ; priority(first_building_kind, Diversity) ),
    ( preferred_kind(Preferred), Preferred == Kind ->
        priority(preferred_building, Preference)
    ; Preference = 0 ),
    ( Points >= Need -> priority(victory_build, Score), Reason = clinch_victory
    ; Score = Base, Reason = expand_settlement ),
    Utility is Score + Diversity + Preference + PointWeight*Points - CostWeight*(CostWood+CostStone),
    build_location(Kind, Excluded, Target, Route),
    Candidate = candidate(Target, Reason, Utility, Route).
development(situation(_, _, Wood, _, _, Excluded), candidate(Target, shield_settlement, Score, Route)) :-
    defensive_fence(Wood, Excluded, Target, Route),
    priority(fence, Score).
development(situation(Need, _, Wood, Stone, _, Excluded), candidate(Target, Reason, Score, Route)) :-
    ( tool_plan(Need, Wood, Stone, Excluded, Intention),
      ( Intention = craft(_) ->
          Target = Intention, Route = [], Reason = improve_tools, priority(craft, Score)
      ; action_goal(Intention, Target), path_to(Target, Route), is_list(Route),
        Reason = tool_materials, priority(gather, Base), priority(tool_supply, Bonus),
        length(Route, Steps), priority(route_step, Cost),
        Score is Base+Bonus+Cost*Steps )
    ; gather_plan(Need, Wood, Stone, Excluded, Target, Route),
      Reason = construction_materials, priority(gather, Score) ).

action_goal(reposition(_, Target), Target) :- !.
action_goal(Target, Target).

repair_proposal(situation(_, _, Wood, _, _, Excluded), Candidate) :-
    Wood >= 1, bot_identity(Id, _),
    allied_building(building(UUID, X, Y, Kind, _, Id, HP, MaxHP)),
    string(UUID), integer(HP), integer(MaxHP), HP < MaxHP,
    atom_string(Type, Kind), building_spec(Type, _, _, Points),
    Target = repair(UUID), available(Excluded, Target),
    priority(repair, Base),
    ( HP =< 10 -> priority(critical_repair, Urgency) ; Urgency = 0 ),
    ( incoming_bandit(X, Y) -> Risk = 18
    ; nearby_opponent(X, Y) -> Risk = 12
    ; Risk = 0 ),
    Value is Base + Urgency + Risk + Points + 18*(MaxHP-HP)/MaxHP,
    make_routed(Target, damaged_structure, Value, Candidate).

raid(situation(_, Theirs, _, _, HP, Excluded), Candidate) :-
    priority(critical_hp, Critical), HP > Critical,
    enemy_building(building(UUID, X, Y, Kind, _, _, BuildingHP, _)),
    string(UUID), integer(BuildingHP), BuildingHP > 0,
    atom_string(Type, Kind), building_spec(Type, _, _, Points),
    Target = attack(UUID), available(Excluded, Target),
    priority(enemy_urgent_score, Urgent),
    bot_position(BX, BY), distance(BX, BY, X, Y, D),
    Hits is (BuildingHP+9)//10,
    ( Theirs >= Urgent -> priority(urgent_raid, Base), Reason = deny_enemy_victory
    ; D+Hits =< 9, priority(raid, Base), Reason = exposed_enemy_structure ),
    ( hostile_near(X, Y, 3, _) -> Risk = 18 ; Risk = 0 ),
    Value is Base+2*Points-2*Hits-Risk,
    make_routed(Target, Reason, Value, Candidate).

make_routed(Target, Reason, Value, Result) :-
    path_to(Target, Route),
    ( is_list(Route) -> Result = candidate(Target, Reason, Value, Route)
    ; Result = unreachable(Target) ).

defence(situation(_, _, _, _, HP, Excluded), Candidate) :-
    bot_position(X, Y), local_pressure(X, Y, Count, Nearest),
    ( should_retreat(HP, Count, Nearest),
      retreat_plan(X, Y, Excluded, Candidate)
    ; \+ should_retreat(HP, Count, Nearest),
      live_bandit(Id, BX, BY, BanditHP, State, Direction),
      Target = attack_bandit(Id), available(Excluded, Target),
      distance(X, Y, BX, BY, D),
      ( D =< 2, priority(self_defence, Base), Reason = immediate_bandit
      ; priority(intercept_radius, Radius), D =< Radius,
        threatens_structure(BX, BY, State, Direction, Points, BuildingHP),
        priority(intercept, Defence),
        Base is Defence+Points/2+max(0, 25-BuildingHP), Reason = defend_settlement ),
      Value is Base + (100-BanditHP)/10-8*max(0, Count-1),
      make_routed(Target, Reason, Value, Candidate),
      ( Candidate = candidate(_, _, _, Route) ->
          priority(intercept_steps, Limit), length(Route, Steps), Steps =< Limit
      ; true ) ).

live_bandit(Id, X, Y, HP, State, Direction) :-
    bandit(Id, X, Y, HP, _, State, Direction), HP > 0.

hostile_near(X, Y, Radius, D) :-
    live_bandit(_, BX, BY, _, _, _), distance(X, Y, BX, BY, D), D =< Radius.

local_pressure(X, Y, Count, Nearest) :-
    priority(threat_radius, Radius),
    findall(D, hostile_near(X, Y, Radius, D), Distances),
    length(Distances, Count),
    ( Distances == [] -> Nearest is Radius+1 ; min_list(Distances, Nearest) ).

should_retreat(HP, Count, Nearest) :-
    priority(critical_hp, Threshold),
    ( HP =< Threshold, Nearest =< 4 ; Count >= 2, Nearest =< 3, HP =< 60 ).

immediate_danger :-
    bot_position(X, Y), bot_health(HP), local_pressure(X, Y, Count, Nearest),
    ( should_retreat(HP, Count, Nearest) ; Nearest =< 1 ).

incoming_bandit(X, Y) :- once(hostile_near(X, Y, 4, _)).

nearby_opponent(X, Y) :-
    bot_identity(_, Team), opposing_team(Team, Enemy),
    player(Id, EX, EY, Enemy, _), online_player(Id), \+ downed(Id),
    distance(X, Y, EX, EY, D), D =< 4.

% Infer the threatened building from public position, direction and stance.
threatens_structure(X, Y, State, Direction, Points, HP) :-
    memberchk(State, ["chase", "attack"]),
    allied_building(building(_, AX, AY, Name, _, _, HP, _)),
    integer(HP), atom_string(Kind, Name), building_spec(Kind, _, _, Points),
    settlement_pathfinding:footprint(Kind, AX, AY, Cells),
    findall(D, (member(cell(CX, CY), Cells), distance(X, Y, CX, CY, D)), Distances),
    min_list(Distances, Distance), Distance =< 4,
    ( Distance =:= 1
    ; atom_string(Facing, Direction), settlement_pathfinding:delta(Facing, DX, DY),
      NX is X+DX, NY is Y+DY,
      once((member(cell(CX, CY), Cells), distance(NX, NY, CX, CY, Next), Next < Distance)) ).

% Keep escaping while a pursuer is still within the recovery radius.
% Reaching the normal retreat trigger boundary must not reverse the route.
continue_retreat(HP, Previous, Excluded, Candidate) :-
    Previous = cell(_, _),
    priority(critical_hp, Critical), HP =< Critical,
    bot_position(X, Y), priority(recovery_radius, Radius),
    once(hostile_near(X, Y, Radius, _)),
    retreat_plan(X, Y, Excluded, Candidate).

retreat_plan(X, Y, Excluded, Candidate) :-
    findall(Key-cell(TX, TY),
            ( between(-5, 5, DX), between(-5, 5, DY),
              D is abs(DX)+abs(DY), between(2, 5, D),
              TX is X+DX, TY is Y+DY,
              world_size(W, H), TX >= 0, TY >= 0, TX < W, TY < H,
              available(Excluded, cell(TX, TY)),
              priority(recovery_radius, Radius),
              \+ hostile_near(TX, TY, 4, _),
              ( hostile_near(TX, TY, Radius, _) -> Key is D+10 ; Key is D ) ),
            Cells),
    keysort(Cells, Sorted),
    priority(retreat, Score),
    ( once((member(_-Target, Sorted), path_to(Target, Route), is_list(Route),
            length(Route, Steps), Steps =< 8, safe_initial_step(X, Y, Route))) ->
        Candidate = candidate(Target, evade_bandits, Score, Route)
    ; live_bandit(Id, BX, BY, _, _, _), distance(X, Y, BX, BY, 1),
      Target = attack_bandit(Id), available(Excluded, Target),
      priority(self_defence, Defence),
      Candidate = candidate(Target, cornered, Defence, [])
    ).

safe_initial_step(X, Y, [Direction|_]) :-
    settlement_pathfinding:delta(Direction, DX, DY), NX is X+DX, NY is Y+DY,
    priority(recovery_radius, Radius),
    nearest_threat(X, Y, Radius, Before),
    nearest_threat(NX, NY, Radius, After), After >= Before.

nearest_threat(X, Y, Radius, Distance) :-
    Far is Radius+1,
    findall(D, hostile_near(X, Y, Radius, D), Distances),
    min_list([Far|Distances], Distance).

resource_safe(X, Y) :-
    ( bot_health(HP), priority(critical_hp, Critical), HP =< Critical ->
        priority(recovery_radius, Radius),
        \+ hostile_near(X, Y, Radius, _)
    ; true ).

safe_gather_route(Route) :-
    ( bot_health(HP), priority(critical_hp, Critical), HP =< Critical ->
        route_exposure(Route, 0)
    ; true ).

distance(X, Y, TX, TY, D) :- D is abs(X-TX)+abs(Y-TY).

combat_target(attack(_)).
combat_target(attack_bandit(_)).


opposing_team(blue, red).
opposing_team(red, blue).

available(Excluded, Target) :- \+ memberchk(Target, Excluded).

routed_action([], Target, Target).
routed_action([Direction | _], Target, reposition(Direction, Target)).

attack_action([], _, Cooldown, wait(Cooldown)) :- Cooldown > 0, !.
attack_action(Route, Target, _, Action) :- routed_action(Route, Target, Action).

% Keep costs and points aligned with settlement.buildings/catalog.
building_spec(fence, 2, 0, 0).
building_spec(house, 15, 10, 15).
building_spec(watchtower, 10, 8, 10).
building_spec(sawmill, 20, 10, 20).
building_spec(mine, 10, 20, 20).

settlement_kind(house).
settlement_kind(watchtower).
settlement_kind(sawmill).
settlement_kind(mine).

owned_structure(Kind, X, Y) :-
    bot_identity(Id, Team),
    building(UUID, X, Y, Kind, Team, Id, _, _),
    string(UUID).

preferred_kind(Kind) :-
    missing_kind(Kind), !.
preferred_kind(sawmill).

missing_kind(Kind) :-
    settlement_kind(Kind), atom_string(Kind, Name), \+ owned_structure(Name, _, _).

defensive_fence(Wood, Excluded, Target, Route) :-
    Wood >= 2,
    owned_structure(Kind, X, Y), Kind \== "fence",
    once(threat_direction(X, Y, _, _)),
    \+ (owned_structure("fence", FX, FY), abs(X-FX)+abs(Y-FY) =< 4),
    build_location(fence, Excluded, Target, Route),
    !.

build_location(Kind, Excluded, Target, Route) :-
    findall(X-Y, (owned_structure(Name, X, Y), Name \== "fence"), Settlement),
    ( build_near(Kind, Excluded, Settlement, Target, Route) -> true
    ; Kind \== fence, Settlement \== [],
      build_near(Kind, Excluded, [], Target, Route)
    ).

build_near(Kind, Excluded, Settlement, Target, Route) :-
    bot_position(BX, BY),
    world_size(Width, Height),
    construction_terrain(Terrain),
    settlement_pathfinding:building_extent(Kind, SpanX, SpanY),
    OffsetX is SpanX - 1, OffsetY is SpanY - 1,
    MaxX is Width - SpanX, MaxY is Height - SpanY,
    findall(key(Lower, Distance, X, Y)-Candidate,
            ( between(0, MaxX, X), between(0, MaxY, Y),
              near_settlement(Kind, Settlement, X, Y),
              Candidate = build(Kind, X, Y), available(Excluded, Candidate),
              Distance is max(0, max(X - BX, BX - X - OffsetX)) +
                            max(0, max(Y - BY, BY - Y - OffsetY)),
              ( Distance =:= 0 -> Lower = 1 ; Lower is max(0, Distance - 2) )
            ), Unsorted),
    keysort(Unsorted, Candidates),
    shortest_build(Candidates, Terrain, none, best(_, Target, Route)).

near_settlement(_, [], _, _) :- !.
near_settlement(fence, Settlement, X, Y) :- !,
    member(SX-SY, Settlement),
    abs(X-SX)+abs(Y-SY) =< 4,
    threat_direction(SX, SY, EX, EY),
    abs(X-EX)+abs(Y-EY) < abs(SX-EX)+abs(SY-EY),
    \+ (owned_structure("fence", FX, FY), abs(SX-FX)+abs(SY-FY) =< 4),
    !.
near_settlement(_, Settlement, X, Y) :-
    once((member(SX-SY, Settlement), abs(X-SX)+abs(Y-SY) =< 8)).

threat_direction(X, Y, EX, EY) :-
    ( enemy_building(building(_, EX, EY, _, _, _, _, _)), Radius = 8
    ; live_bandit(_, EX, EY, _, State, _), memberchk(State, ["attack", "chase"]), Radius = 6 ),
    distance(X, Y, EX, EY, D), D =< Radius.

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

tool_plan(Need, Wood, Stone, Excluded, Action) :-
    tool_recipe(Tool, CostWood, CostStone),
    Need >= 30,
    inventory_item(Tool, 0),
    available(Excluded, craft(Tool)),
    ( Wood < CostWood ->
        choose_resource_type(tree, Excluded, Target, Route),
        routed_action(Route, Target, Action)
    ; Stone < CostStone ->
        choose_resource_type(stone, Excluded, Target, Route),
        routed_action(Route, Target, Action)
    ; Action = craft(Tool)
    ), !.

tool_recipe(axe, 3, 2).
tool_recipe(pickaxe, 2, 3).

gather_plan(Need, Wood, Stone, Excluded, Target, Route) :-
    ( Need =< 15 -> Kind = house ; preferred_kind(Kind) ),
    building_spec(Kind, CostWood, CostStone, _),
    ( Wood < CostWood, Stone < CostStone -> choose_resource(Excluded, Target, Route)
    ; Wood < CostWood -> choose_resource_type(tree, Excluded, Target, Route)
    ; Stone < CostStone -> choose_resource_type(stone, Excluded, Target, Route)
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
              resource_safe(X, Y),
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
    ; ( path_to(Target, Route), is_list(Route), safe_gather_route(Route), length(Route, Steps),
        Key = key(Steps, Rank, X, Y),
        ( Best == none ; Best = best(OldKey, _, _), Key @< OldKey ) ->
          Next = best(Key, Target, Route)
      ; Next = Best
      ),
      shortest_resource(Rest, Next, Result)
    ).
