:- module(settlement_dcg, [parse_frame/2, frame//1]).
:- encoding(utf8).

:- meta_predicate field(+, +, 2, ?, ?).
:- meta_predicate list_of(3, ?, ?, ?).
:- meta_predicate list_items(3, ?, ?, ?).
:- meta_predicate list_tail(3, ?, ?, ?).
:- meta_predicate optional(3, ?, ?, ?).

% parse_frame(+Line, -Result)
% Line is a string without its line terminator. Result is ok(Frame) or
% error(Reason). A server rejection is a valid frame: ok(reply(1, error(...))).
parse_frame(Line, Result) :-
    ( string(Line) ->
        string_codes(Line, Codes),
        ( phrase(frame(Frame), Codes) -> Result = ok(Frame)
        ; phrase(header(_, Version), Codes, _), Version \== "1" ->
            Result = error(unsupported_version(Version))
        ; Result = error(invalid_frame)
        )
    ; Result = error(invalid_input)
    ).

frame(reply(1, Reply)) --> header(reply, "1"), reply(Reply).
frame(event(1, Version, Seq, Changes, Resync)) -->
    header(event, "1"),
    " version=", decimal(Version),
    " seq=", decimal(Seq), { Version =:= Seq },
    " changes=", list_of(token, Changes),
    " resync=", boolean(Resync).

header(reply, Version) --> "reply v=", version(Version).
header(event, Version) --> "event v=", version(Version).

version(Version) -->
    digits(Codes), { string_codes(Version, Codes) }, version_boundary.

version_boundary([], []).
version_boundary([32 | Rest], [32 | Rest]).

reply(ok(Payload)) --> " ok ", success(Payload).
reply(error(Code, Detail)) -->
    " error code=", token(Code), error_detail(Code, Detail).

success(move(X, Y)) --> "move", position(X, Y).
success(Inventory) --> "inventory", inventory_fields("", Inventory).
success(gather_start(Progress)) --> "gather-start", progress(Progress).
success(gather_heartbeat(Progress)) --> "gather-heartbeat", progress(Progress).
success(gather_finish(Id, Resource, Amount, X, Y)) -->
    "gather-finish operation_id=", uuid(Id),
    " resource=", gathered_resource(Resource),
    " amount=", decimal(Amount), position(X, Y).
success(gather_cancel(Id, Attempt)) -->
    "gather-cancel operation_id=", optional(uuid, Id),
    " attempt_id=", optional(uuid, Attempt).
success(build(Building)) --> "build building=", building(Building).
success(attack_building(Seq, X, Y, Id, HP, Destroyed)) -->
    "attack-building attack_seq=", decimal(Seq), position(X, Y),
    " building_id=", uuid(Id), " hp=", decimal(HP),
    " destroyed=", boolean(Destroyed).
success(repair_building(Seq, X, Y, Id, HP, Restored)) -->
    "repair-building repair_seq=", decimal(Seq), position(X, Y),
    " building_id=", uuid(Id), " hp=", decimal(HP),
    " restored_hp=", decimal(Restored).
success(World) --> "world", world(World).
success(world_versioned(Version, World, Online, Gathering)) -->
    "world-versioned version=", decimal(Version), world(World),
    " online=", list_of(decimal, Online),
    " gathering=", list_of(decimal, Gathering).
success(subscribe_events(Since, Version)) -->
    "subscribe-events since=", decimal(Since), " version=", decimal(Version).
success(unsubscribe_events) --> "unsubscribe-events".

position(X, Y) --> " x=", decimal(X), " y=", decimal(Y).

progress(progress(Id, Attempt, Duration, Elapsed)) -->
    " operation_id=", uuid(Id), " attempt_id=", uuid(Attempt),
    " duration_ms=", decimal(Duration), " elapsed_ms=", decimal(Elapsed).

error_detail("too-early", Progress) --> !, progress(Progress).
error_detail(Code, last_attack_seq(Seq)) -->
    { memberchk(Code, ["stale-attack", "attack-seq-conflict"]) }, !,
    " last_attack_seq=", decimal(Seq).
error_detail(Code, last_repair_seq(Seq)) -->
    { memberchk(Code, ["stale-repair", "repair-seq-conflict"]) }, !,
    " last_repair_seq=", decimal(Seq).
error_detail(_, none) --> [].

world(world(size(Width, Height), Self, Players, Objects, totals(Wood, Stone),
            score(Blue, Red), match(Phase, Winner), Inventory, sequences(Attack, Repair))) -->
    " width=", decimal(Width), " height=", decimal(Height), " self=", decimal(Self),
    " players=", list_of(player, Players), " objects=", list_of(object, Objects),
    " total_wood=", decimal(Wood), " total_stone=", decimal(Stone),
    " blue=", decimal(Blue), " red=", decimal(Red),
    " phase=", phase(Phase), " winner=", optional(team, Winner),
    inventory_fields("inventory_", Inventory),
    " attack_seq=", decimal(Attack), " repair_seq=", decimal(Repair).

inventory_fields(Prefix, inventory(Wood, Stone, Axe, Pickaxe)) -->
    field(Prefix, "wood", decimal(Wood)), field(Prefix, "stone", decimal(Stone)),
    field(Prefix, "axe", decimal(Axe)), field(Prefix, "pickaxe", decimal(Pickaxe)).

field(Prefix, Name, Rule) -->
    " ", literal(Prefix), literal(Name), "=", call(Rule).

literal(String) --> { string_codes(String, Codes) }, codes(Codes).

codes([]) --> [].
codes([Code | Rest]) --> [Code], codes(Rest).

player(player(Id, X, Y, Team, Name)) -->
    "player(", decimal(Id), ",", decimal(X), ",", decimal(Y), ",",
    team(Team), ",", quoted(Name), ")".

object(resource(X, Y, Type)) -->
    "resource(", decimal(X), ",", decimal(Y), ",", resource_type(Type), ")".
object(obstacle(X, Y, Kind)) -->
    "obstacle(", decimal(X), ",", decimal(Y), ",", token(Kind), ")".
object(Building) --> building(Building).

building(building(Id, X, Y, Kind, Team, Owner, HP, MaxHP)) -->
    "building(", decimal(X), ",", decimal(Y), ",", optional(uuid, Id), ",",
    token(Kind), ",", optional(team, Team), ",", optional(decimal, Owner), ",",
    optional(decimal, HP), ",", optional(decimal, MaxHP), ")".

resource_type(tree) --> "tree".
resource_type(stone) --> "stone".
gathered_resource(wood) --> "wood".
gathered_resource(stone) --> "stone".
team(blue) --> "blue".
team(red) --> "red".
phase(active) --> "active".
phase(finished) --> "finished".
boolean(true) --> "true".
boolean(false) --> "false".

optional(_, none) --> "none".
optional(Rule, Value) --> call(Rule, Value).

list_of(Rule, Values) --> "[", list_items(Rule, Values).
list_items(_, []) --> "]", !.
list_items(Rule, [Value | Rest]) --> call(Rule, Value), list_tail(Rule, Rest).
list_tail(_, []) --> "]", !.
list_tail(Rule, [Value | Rest]) -->
    ",", call(Rule, Value), list_tail(Rule, Rest).

decimal(Number) --> digits(Codes), { number_codes(Number, Codes) }.

digits([Digit | Rest]) -->
    [Digit], { ascii_digit(Digit) }, digit_tail(Rest).
digit_tail([Digit | Rest]) -->
    [Digit], { ascii_digit(Digit) }, !, digit_tail(Rest).
digit_tail([]) --> [].

ascii_digit(Code) :- between(0'0, 0'9, Code).
ascii_lower(Code) :- between(0'a, 0'z, Code).

token(Token) -->
    [First], { token_character(First) }, token_tail(Rest),
    { string_codes(Token, [First | Rest]) }.
token_tail([Code | Rest]) -->
    [Code], { token_character(Code) }, !, token_tail(Rest).
token_tail([]) --> [].

token_character(Code) :-
    ( ascii_lower(Code) ; ascii_digit(Code) ; Code =:= 0'- ).

uuid(UUID) -->
    hex_group(8, Codes, [0'- | Second]), "-",
    hex_group(4, Second, [0'- | Third]), "-",
    hex_group(4, Third, [0'- | Fourth]), "-",
    hex_group(4, Fourth, [0'- | Fifth]), "-",
    hex_group(12, Fifth, []),
    { string_codes(UUID, Codes) }.

hex_group(0, Tail, Tail) --> !.
hex_group(Count, [Code | Rest], Tail) -->
    [Code], { hex_value(Code, _), Left is Count - 1 },
    hex_group(Left, Rest, Tail).

hex_value(Code, Value) :-
    ( ascii_digit(Code) -> Value is Code - 0'0
    ; between(0'a, 0'f, Code), Value is Code - 0'a + 10
    ).

quoted(Name) --> [34], quoted_codes(Codes), { string_codes(Name, Codes) }.
quoted_codes([]) --> [34], !.
quoted_codes([Code | Rest]) -->
    quoted_character(Code), quoted_codes(Rest).

quoted_character(Code) --> [92], !, escape(Code).
quoted_character(Code) -->
    [Code], { Code >= 32, Code =\= 34, \+ unicode_escape_required(Code) }.

escape(34) --> [34].
escape(92) --> [92].
escape(Code) -->
    "u", [A, B, C, D],
    { hex_value(A, High), hex_value(B, Upper),
      hex_value(C, Lower), hex_value(D, Low),
      Code is ((High * 16 + Upper) * 16 + Lower) * 16 + Low,
      ( Code < 32 ; unicode_escape_required(Code) )
    }.

unicode_escape_required(0x85).
unicode_escape_required(0x2028).
unicode_escape_required(0x2029).
