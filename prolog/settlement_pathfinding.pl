:- module(settlement_pathfinding, [path_to/2, construction_terrain/1, preserves_access/2]).

:- use_module(library(assoc)).
:- use_module(library(heaps)).
:- use_module(library(ordsets)).
:- use_module(settlement_knowledge,
              [bot_identity/2, player/5, world_size/2, match_state/2,
               obstacle/3, building/8, known_resource/3, bandit/7, downed/1, reserved/2]).

% Plan against the terrain with every known resource restored. Players
% occupy movement cells but do not block the server's connectivity search.
construction_terrain(terrain(Map, Resources)) :-
    world_size(Width, Height),
    findall(cell(X, Y), known_resource(X, Y, _), ResourceCells),
    sort(ResourceCells, Resources),
    findall(Cell-true, (member(Cell, Resources); structure_cell(Cell)), Blocked),
    sort(Blocked, Sorted),
    list_to_assoc(Sorted, Occupied),
    Map = map(Width, Height, Occupied).

preserves_access(terrain(Map, Resources), build(Kind, X, Y)) :-
    footprint(Kind, X, Y, Cells),
    \+ (member(cell(CX, CY), Cells), reserved(CX, CY)),
    maplist(walkable(Map), Cells),
    Map = map(Width, Height, Occupied),
    foldl(block_cell, Cells, Occupied, WithBuilding),
    After = map(Width, Height, WithBuilding),
    findall(Neighbor, (member(Cell, Cells), step(Cell, _, Neighbor)), Adjacent),
    sort(Adjacent, Boundary),
    ord_intersection(Boundary, Resources, AffectedResources),
    forall(member(Resource, AffectedResources), has_exit(After, Resource)),
    include(walkable(After), Boundary, Exits),
    connected_exits(After, Exits).

block_cell(Cell, Occupied, WithCell) :-
    put_assoc(Cell, Occupied, true, WithCell).

has_exit(Map, Cell) :-
    once((step(Cell, _, Exit), walkable(Map, Exit))).

% Any paths severed by removing a footprint must have used its boundary.
% If the remaining exits still communicate, the placement splits no region.
connected_exits(Map, [First | Rest]) :-
    empty_assoc(Empty),
    put_assoc(First, Empty, true, Seen),
    reachable_exits(Map, [First], [], Seen, Rest).

reachable_exits(_, _, _, _, []) :- !.
reachable_exits(Map, [Cell | Front], Back, Seen, Goals) :-
    !,
    ord_del_element(Goals, Cell, Remaining),
    findall(Next, (step(Cell, _, Next), walkable(Map, Next)), Neighbors),
    foldl(enqueue_cell, Neighbors, queue(Back, Seen), queue(NextBack, NextSeen)),
    reachable_exits(Map, Front, NextBack, NextSeen, Remaining).
reachable_exits(Map, [], Back, Seen, Goals) :-
    Back = [_ | _],
    reverse(Back, Front),
    reachable_exits(Map, Front, [], Seen, Goals).

enqueue_cell(Cell, queue(Back, Seen), Queue) :-
    ( get_assoc(Cell, Seen, _) -> Queue = queue(Back, Seen)
    ; put_assoc(Cell, Seen, true, Next), Queue = queue([Cell | Back], Next)
    ).

% path_to(+Target, -Route)
% Target: cell(X,Y), gather(tree|stone,X,Y), build(fence|house,X,Y),
% attack(UUID), or repair(UUID). UUIDs are strings as in the snapshot.
% Route is a list of directions, [] when already there, or no_path.
% Build plans where to stand for a proposed anchor. The server still
% validates protected areas, access, and costs when executing the command.
path_to(Target, Route) :-
    ( ground(Target),
      snapshot(plan(Target, Start, Map, Goals)),
      astar(Start, Map, Goals, Directions) ->
        Route = Directions
    ; Route = no_path
    ).

plan(Target, Start, Map, Goals) :-
    match_state(active, _),
    bot_identity(Id, Team),
    \+ downed(Id),
    player(Id, X, Y, Team, _),
    Start = cell(X, Y),
    world_size(Width, Height),
    findall(Cell-true, blocked_cell(Id, Cell), BlockedPairs),
    sort(BlockedPairs, Sorted),
    list_to_assoc(Sorted, Blocked),
    Map = map(Width, Height, Blocked),
    target_cells(Target, Team, Map, Candidates),
    include(walkable(Map), Candidates, Goals),
    Goals = [_ | _].

blocked_cell(_, cell(X, Y)) :- settlement_knowledge:resource(X, Y, _).
blocked_cell(Self, cell(X, Y)) :- player(Id, X, Y, _, _), Id \== Self, \+ downed(Id).
blocked_cell(_, cell(X, Y)) :- bandit(_, X, Y, HP, _, _, _), HP > 0.
blocked_cell(_, Cell) :- structure_cell(Cell).

structure_cell(cell(X, Y)) :- obstacle(X, Y, _).
structure_cell(cell(X, Y)) :- building(_, X, Y, _, _, _, _, _).
structure_cell(Cell) :-
    building(_, X, Y, Kind, _, _, _, _),
    footprint(Kind, X, Y, Cells),
    member(Cell, Cells).

target_cells(cell(X, Y), _, _, [cell(X, Y)]).
target_cells(gather(Type, X, Y), _, _, Goals) :-
    settlement_knowledge:resource(X, Y, Type),
    interaction_cells([cell(X, Y)], 1, Goals).
target_cells(build(Kind, X, Y), _, Map, Goals) :-
    footprint(Kind, X, Y, Cells),
    maplist(walkable(Map), Cells),
    interaction_cells(Cells, 2, Goals).
target_cells(attack(UUID), Team, _, Goals) :-
    string(UUID),
    building(UUID, X, Y, Kind, Enemy, _, _, _),
    memberchk(Enemy, [blue, red]),
    Enemy \== Team,
    footprint(Kind, X, Y, Cells),
    interaction_cells(Cells, 1, Goals).
target_cells(attack_bandit(UUID), _, _, Goals) :-
    bandit(UUID, X, Y, HP, _, _, _), HP > 0,
    interaction_cells([cell(X, Y)], 1, Goals).
target_cells(repair(UUID), Team, _, Goals) :-
    string(UUID),
    building(UUID, X, Y, Kind, Team, _, _, _),
    footprint(Kind, X, Y, Cells),
    interaction_cells(Cells, 1, Goals).

footprint(Kind, X, Y, Cells) :-
    % DCG stores building kinds as strings. The public build target uses atoms.
    integer(X), integer(Y),
    ( string(Kind) -> atom_string(Type, Kind) ; Type = Kind ),
    building_extent(Type, Width, Height),
    MaxX is X + Width - 1,
    MaxY is Y + Height - 1,
    findall(cell(CX, CY), (between(X, MaxX, CX), between(Y, MaxY, CY)), Cells).

building_extent(fence, 1, 1).
building_extent(house, 2, 2).
building_extent(watchtower, 2, 1).
building_extent(sawmill, 2, 2).
building_extent(mine, 2, 1).

interaction_cells(Footprint, Radius, Cells) :-
    MinOffset is -Radius,
    findall(cell(X, Y),
            ( member(cell(FX, FY), Footprint),
              between(MinOffset, Radius, DX),
              Remaining is Radius - abs(DX),
              MinY is -Remaining,
              between(MinY, Remaining, DY),
              X is FX + DX, Y is FY + DY,
              \+ memberchk(cell(X, Y), Footprint)
            ),
            Candidates),
    sort(Candidates, Cells).

walkable(map(Width, Height, Blocked), Cell) :-
    Cell = cell(X, Y),
    integer(X), integer(Y),
    X >= 0, X < Width, Y >= 0, Y < Height,
    \+ get_assoc(Cell, Blocked, _).

step(cell(X, Y), Direction, cell(NX, NY)) :-
    delta(Direction, DX, DY),
    NX is X + DX, NY is Y + DY.

delta(up, 0, -1).
delta(down, 0, 1).
delta(left, -1, 0).
delta(right, 1, 0).

astar(Start, Map, Goals, Route) :-
    heuristic(Start, Goals, H),
    empty_heap(EmptyOpen),
    add_to_heap(EmptyOpen, H-H, entry(Start, 0), Open),
    empty_assoc(EmptyVisited),
    put_assoc(Start, EmptyVisited, node(0, none, none), Visited),
    search(Open, Visited, Map, Goals, Route).

search(Open, Visited, Map, Goals, Route) :-
    % An empty heap fails. An entry superseded by a shorter route is skipped.
    get_from_heap(Open, _, entry(Cell, Cost), Remaining),
    get_assoc(Cell, Visited, node(Best, _, _)),
    ( Cost > Best ->
        search(Remaining, Visited, Map, Goals, Route)
    ; memberchk(Cell, Goals) ->
        restore_route(Cell, Visited, [], Route)
    ; findall(Direction-Next,
              (step(Cell, Direction, Next), walkable(Map, Next)),
              Neighbors),
      NextCost is Cost + 1,
      foldl(relax(Cell, NextCost, Goals), Neighbors,
            frontier(Remaining, Visited), frontier(NextOpen, NextVisited)),
      search(NextOpen, NextVisited, Map, Goals, Route)
    ).

relax(Previous, Cost, Goals, Direction-Cell,
      frontier(Open, Visited), frontier(NextOpen, NextVisited)) :-
    ( get_assoc(Cell, Visited, node(Known, _, _)), Known =< Cost ->
        NextOpen = Open, NextVisited = Visited
    ; heuristic(Cell, Goals, H),
      Priority is Cost + H,
      add_to_heap(Open, Priority-H, entry(Cell, Cost), NextOpen),
      put_assoc(Cell, Visited, node(Cost, Previous, Direction), NextVisited)
    ).

heuristic(cell(X, Y), [cell(GX, GY) | Goals], H) :-
    First is abs(GX - X) + abs(GY - Y),
    foldl(nearer_goal(X, Y), Goals, First, H).

nearer_goal(X, Y, cell(GX, GY), Best, Distance) :-
    Distance is min(Best, abs(GX - X) + abs(GY - Y)).

restore_route(Cell, Visited, Tail, Route) :-
    get_assoc(Cell, Visited, node(Cost, Previous, Direction)),
    ( Cost =:= 0 -> Route = Tail
    ; restore_route(Previous, Visited, [Direction | Tail], Route)
    ).
