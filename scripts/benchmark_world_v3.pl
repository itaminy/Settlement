:- use_module('../prolog/settlement_client').
:- use_module('../prolog/settlement_dcg').
:- use_module('../prolog/settlement_knowledge', [load_snapshot/1, world_size/2]).
:- use_module('../prolog/settlement_pathfinding').
:- initialization(main, main).

main :-
    with_text_session(9001, register("Benchmark"), 10, _, measure_snapshots),
    world_size(100, 100),
    findall(Ms, (between(1, 40, _), timed_path(Ms, _)), Times),
    timed_path(_, Steps),
    report("A* from red spawn to blue spawn", Times),
    format('route_steps=~d~n', [Steps]).

measure_snapshots(In, Out) :-
    settlement_client:request_snapshot(_, In, Out),
    findall(Ms, (between(1, 50, _), timed_snapshot(In, Out, Ms, _)), Times),
    timed_snapshot(In, Out, _, Bytes),
    report("TCP snapshot and DCG decode", Times),
    format('snapshot_bytes_without_lf=~d~n', [Bytes]).

timed_snapshot(In, Out, Ms, Bytes) :-
    get_time(Start),
    settlement_client:request_snapshot(Line, In, Out),
    parse_frame(Line, ok(Frame)),
    load_snapshot(Frame),
    get_time(End),
    Ms is (End-Start)*1000,
    string_bytes(Line, Utf8, utf8),
    length(Utf8, Bytes).

timed_path(Ms, Steps) :-
    get_time(Start),
    path_to(cell(23, 25), Route),
    length(Route, Steps),
    get_time(End),
    Ms is (End-Start)*1000.

report(Label, Times) :-
    msort(Times, Sorted),
    length(Sorted, Count),
    Count > 0,
    Half is (Count+1)//2,
    Tail is ceiling(Count*0.95),
    nth1(Half, Sorted, Median),
    nth1(Tail, Sorted, P95),
    format('~s n=~d median_ms=~2f p95_ms=~2f~n', [Label, Count, Median, P95]).
