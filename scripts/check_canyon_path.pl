:- encoding(utf8).
:- use_module('../prolog/settlement_client').
:- use_module('../prolog/settlement_dcg').
:- use_module('../prolog/settlement_knowledge', except([resource/3])).
:- use_module('../prolog/settlement_pathfinding').
:- initialization(main, main).

main :-
    with_text_session(9100, register("Canyon A star"), 10, _, verify).

verify(In, Out) :-
    send_line(Out, "покажи мир с версией"),
    settlement_client:read_snapshot(In, Line),
    parse_frame(Line, ok(Frame)),
    load_snapshot(Frame),
    get_time(Start),
    path_to(cell(74,66), Approach),
    construction_terrain(terrain(Map, _)),
    settlement_pathfinding:astar(cell(74,66), Map, [cell(82,92)], Crossing),
    settlement_pathfinding:astar(cell(85,68), Map, [cell(89,68)], West),
    settlement_pathfinding:astar(cell(95,79), Map, [cell(95,83)], East),
    length(West, 4), length(East, 4),
    \+ settlement_pathfinding:walkable(Map, cell(87,67)),
    \+ settlement_pathfinding:walkable(Map, cell(97,81)),
    length(Approach, A), length(Crossing, C),
    get_time(End), Time is (End-Start)*1000,
    format('A*: approach=~d canyon=~d west-bridge=4 east-bridge=4 elapsed=~2f ms~n',[A,C,Time]).
