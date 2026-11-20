:- module(settlement_client,
          [fetch_snapshot/4, classify_frame/2, with_text_session/5,
           with_edn_session/5, craft_tool/5, server_port/1, send_line/2, read_line/2]).
:- encoding(utf8).
:- use_module(library(pcre)).
:- use_module(library(readutil)).
:- use_module(library(socket)).
:- use_module(library(time)).

:- meta_predicate with_text_session(+, +, +, -, 2).
:- meta_predicate with_edn_session(+, +, +, -, 2).

% Auth is register(Name) or login(Credentials). Credentials stay in memory.
fetch_snapshot(PlayerId, Auth, Credentials, Snapshot) :-
    with_text_session(PlayerId, Auth, 10, Credentials, request_snapshot(Snapshot)).

with_text_session(PlayerId, Auth, Timeout, Credentials, Goal) :-
    get_time(Now),
    Deadline is Now + Timeout,
    with_edn_session(PlayerId, Auth, Timeout, Credentials, text_mode(Deadline, Goal)).

with_edn_session(PlayerId, Auth, Timeout, Credentials, Goal) :-
    require((integer(PlayerId), PlayerId > 0), invalid_player_id),
    server_port(Port),
    setup_call_cleanup(
        tcp_connect('127.0.0.1':Port, Pair, [bypass_proxy(true)]),
        ( call_with_time_limit(Timeout, handshake(Pair, PlayerId, Auth, Credentials, In, Out)),
          once(call(Goal, In, Out))
        ),
        close(Pair, [force(true)])).

server_port(Port) :-
    ( getenv('SETTLEMENT_PORT', Value) ->
        require((catch(atom_number(Value, Port), _, fail),
                 integer(Port), between(1, 65535, Port)), invalid_server_port)
    ; Port = 5000
    ).

handshake(Pair, PlayerId, Auth, Credentials, In, Out) :-
    stream_pair(Pair, In, Out),
    set_stream(In, encoding(utf8)),
    set_stream(In, timeout(5)),
    set_stream(Out, encoding(utf8)),
    set_stream(Out, newline(posix)),
    request(In, Out, "{:type :hello}", Hello),
    expect_edn(hello, Hello, [":server-instance"-UUID]),
    require(uuid(UUID), invalid_hello),
    unquote(UUID, Instance),
    authorize(In, Out, PlayerId, Instance, Auth, Credentials).

text_mode(Deadline, Goal, In, Out) :-
    get_time(Now),
    Remaining is Deadline - Now,
    ( Remaining > 0 -> call_with_time_limit(Remaining, switch_to_text(In, Out))
    ; throw(time_limit_exceeded)
    ),
    call(Goal, In, Out).

switch_to_text(In, Out) :-
    request(In, Out, "текст 1", Switch),
    expect_edn(switch, Switch, [":mode"-":text", ":version"-"1"]).

% The server exposes crafting in EDN only. Its reply is a status map with
% optional world data. Read the status without interpreting that data;
% the next versioned text snapshot supplies the authoritative inventory.
craft_tool(PlayerId, Tool, In, Out, Result) :-
    require((integer(PlayerId), PlayerId > 0, memberchk(Tool, [axe, pickaxe])),
            invalid_craft_request),
    format(string(Command), '{:type :craft :player-id ~d :tool :~w}', [PlayerId, Tool]),
    request(In, Out, Command, Reply),
    ( re_match('^\\{:status :ok(?:, :world \\{.*\\})?\\}\\z', Reply) ->
        Result = ok
    ; re_matchsub('^\\{:status :error, :code :([a-z0-9-]+)(?:, :world \\{.*\\})?\\}\\z',
                  Reply, Match) ->
        Result = error(Match.1)
    ; throw(settlement_error(invalid_craft_reply))
    ).

request_snapshot(Snapshot, In, Out) :-
    send_line(Out, "покажи мир с версией"),
    read_snapshot(In, Snapshot).

authorize(In, Out, PlayerId, Instance, register(Name),
          credentials(PlayerId, Instance, Token)) :-
    require((string(Name), Name \== "", string_codes(Name, Codes),
             maplist(name_character, Codes)), invalid_name),
    phrase(edn_name(Codes), Escaped),
    format(string(Line), '{:type :register :player-id ~d :name "~s"}',
           [PlayerId, Escaped]),
    request(In, Out, Line, Reply),
    expect_edn(register, Reply, [":server-instance"-UUID, ":token"-Secret]),
    require((uuid(UUID), unquote(UUID, Instance),
             re_match('^"[A-Za-z0-9_-]{43}"$', Secret)), invalid_registration_reply),
    unquote(Secret, Token).
authorize(In, Out, PlayerId, Instance, login(Credentials), Credentials) :-
    require((Credentials = credentials(PlayerId, Previous, Token),
             string(Previous), string(Token), string_length(Token, 43),
             re_match('^[A-Za-z0-9_-]{43}$', Token)),
            invalid_credentials),
    require(Previous == Instance, stale_server),
    format(string(Line), '{:type :login :player-id ~d :token "~s" :server-instance "~s"}',
           [PlayerId, Token, Instance]),
    login_request(In, Out, Line, 10).

login_request(In, Out, Line, Retries) :-
    request(In, Out, Line, Reply),
    ( Retries > 0, edn_error(Reply, "already-connected") ->
        sleep(0.05),
        Left is Retries - 1,
        login_request(In, Out, Line, Left)
    ; expect_edn(login, Reply, [])
    ).

request(In, Out, Line, Reply) :-
    send_line(Out, Line),
    read_line(In, Reply).

send_line(Out, Line) :-
    format(Out, '~s~n', [Line]),
    flush_output(Out).

read_line(In, Line) :-
    read_line_to_string(In, Line),
    require(Line \== end_of_file, disconnected).

% Only scalar EDN maps returned during handshake are recognized.
% Read fields as strings and never pass them to the Prolog term reader.
edn_fields(Line, Fields) :-
    re_match('^\\{[^{}]*\\}$', Line),
    sub_string(Line, 1, _, 1, Body),
    split_string(Body, " ,\t", " ,\t", Tokens),
    pairs(Tokens, Pairs),
    msort(Pairs, Fields).

pairs([], []).
pairs([Key, Value | Tokens], [Key-Value | Fields]) :-
    pairs(Tokens, Fields).

edn_error(Line, Code) :-
    edn_fields(Line, [":code"-Keyword, ":status"-":error"]),
    sub_string(Keyword, 0, 1, _, ":"),
    sub_string(Keyword, 1, _, 0, Code).

expect_edn(Phase, Line, Fields) :-
    ( edn_error(Line, Code) ->
        server_error(Phase, Code)
    ; msort([":status"-":ok" | Fields], Expected),
      require(edn_fields(Line, Expected), invalid_edn_reply(Phase))
    ).

uuid(Quoted) :-
    re_match('^"[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}"$', Quoted).

unquote(Quoted, Value) :-
    sub_string(Quoted, 1, _, 1, Value).

name_character(Code) :-
    Code >= 32,
    \+ between(127, 159, Code).

edn_name([]) --> [].
edn_name([Code | Codes]) -->
    ( { memberchk(Code, [34, 92]) } -> [92, Code] ; [Code] ),
    edn_name(Codes).

classify_frame(Line, Frame) :-
    split_string(Line, " ", "", Parts),
    ( Parts = ["reply", "v=1", "ok", Kind | _],
      re_match('^[a-z][a-z0-9-]*$', Kind) ->
        Frame = reply(ok, Kind)
    ; Parts = ["reply", "v=1", "error", Field | _],
      re_match('^code=[a-z][a-z0-9-]*$', Field) ->
        sub_string(Field, 5, _, 0, Code),
        Frame = reply(error, Code)
    ; Parts = ["event", "v=1", First | _], First \== "" ->
        Frame = event
    ; Frame = invalid
    ).

read_snapshot(In, Snapshot) :-
    read_line(In, Line),
    classify_frame(Line, Frame),
    ( Frame == event ->
        read_snapshot(In, Snapshot)
    ; Frame == reply(ok, "world-versioned"),
      re_match('^reply v=1 ok world-versioned version=[0-9]+ ', Line) ->
        Snapshot = Line
    ; Frame = reply(error, Code) ->
        server_error(snapshot, Code)
    ; throw(settlement_error(invalid_snapshot_reply))
    ).

require(Goal, Error) :-
    ( call(Goal) -> true ; throw(settlement_error(Error)) ).

% Only known public codes may surface in diagnostics.
server_error(Phase, Code) :-
    ( memberchk(Code, ["already-authenticated", "already-connected",
                       "invalid-credentials", "invalid-message", "invalid-registration",
                       "map-full", "match-finished", "not-authenticated", "player-exists",
                       "stale-server", "unsupported-version"]) ->
        SafeCode = Code
    ; SafeCode = "server-error"
    ),
    throw(settlement_error(rejected(Phase, SafeCode))).
