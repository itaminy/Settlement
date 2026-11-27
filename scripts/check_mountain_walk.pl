:- encoding(utf8).
:- use_module('../prolog/settlement_client').
:- use_module('../prolog/settlement_knowledge', except([resource/3])).
:- use_module('../prolog/settlement_dcg').
:- use_module('../prolog/settlement_pathfinding').
:- use_module('../prolog/settlement_bot', []).
:- initialization(main, main).

main([TX, TY]) :-
    atom_number(TX, GoalX), atom_number(TY, GoalY),
    with_text_session(9100, register("Mountain A star walk"), 10, _,
                      ascend(GoalX, GoalY)).

ascend(GoalX, GoalY, In, Out) :-
    send_line(Out, "покажи мир с версией"),
    settlement_client:read_snapshot(In, Line),
    parse_frame(Line, ok(Frame)),
    load_snapshot(Frame),
    bot_position(X, Y),
    path_to(cell(GoalX, GoalY), Route),
    is_list(Route),
    get_time(Start),
    follow(Route, X, Y, In, Out, GoalX, GoalY),
    get_time(End),
    length(Route, Steps),
    Seconds is End - Start,
    format('Prolog TCP summit (~d,~d): ~d accepted moves in ~2f s~n',
           [GoalX, GoalY, Steps, Seconds]).

follow([], X, Y, _, _, X, Y).
follow([Direction|Rest], X, Y, In, Out, GoalX, GoalY) :-
    settlement_bot:movement_command(Direction, Command),
    settlement_pathfinding:delta(Direction, DX, DY),
    NextX is X+DX,
    NextY is Y+DY,
    sleep(0.37),
    send_line(Out, Command),
    settlement_bot:read_reply(In, Reply),
    ( Reply = ok(move(NextX, NextY)) ->
        follow(Rest, NextX, NextY, In, Out, GoalX, GoalY)
    ; throw(error(unexpected_movement_reply(NextX, NextY, Reply), _))
    ).
