:- encoding(utf8).
:- use_module(settlement_dcg).
:- begin_tests(settlement_dcg).

uuid("01234567-89ab-cdef-0123-456789abcdef").

success_case("move x=12 y=8", move(12, 8)).
success_case("inventory wood=3 stone=2 axe=1 pickaxe=0", inventory(3, 2, 1, 0)).
success_case("gather-start operation_id=~s attempt_id=~s duration_ms=3000 elapsed_ms=0",
             gather_start(progress(Id, Id, 3000, 0))) :- uuid(Id).
success_case("gather-heartbeat operation_id=~s attempt_id=~s duration_ms=3000 elapsed_ms=250",
             gather_heartbeat(progress(Id, Id, 3000, 250))) :- uuid(Id).
success_case("gather-finish operation_id=~s resource=wood amount=1 x=12 y=8",
             gather_finish(Id, wood, 1, 12, 8)) :- uuid(Id).
success_case("gather-cancel operation_id=none attempt_id=~s",
             gather_cancel(none, Id)) :- uuid(Id).
success_case("gather-cancel operation_id=~s attempt_id=none",
             gather_cancel(Id, none)) :- uuid(Id).
success_case("build building=building(12,8,~s,fence,red,2,40,50)",
             build(building(Id, 12, 8, "fence", red, 2, 40, 50))) :- uuid(Id).
success_case("attack-building attack_seq=17 x=12 y=8 building_id=~s hp=40 destroyed=false",
             attack_building(17, 12, 8, Id, 40, false)) :- uuid(Id).
success_case("attack-building attack_seq=18 x=12 y=8 building_id=~s hp=0 destroyed=true",
             attack_building(18, 12, 8, Id, 0, true)) :- uuid(Id).
success_case("repair-building repair_seq=4 x=12 y=8 building_id=~s hp=50 restored_hp=10",
             repair_building(4, 12, 8, Id, 50, 10)) :- uuid(Id).
success_case("subscribe-events since=40 version=42", subscribe_events(40, 42)).
success_case("unsubscribe-events", unsubscribe_events).

test(success, [forall(success_case(Format, Term))]) :-
    uuid(Id),
    findall(Id, sub_string(Format, _, 2, _, "~s"), Args),
    format(string(Body), Format, Args),
    string_concat("reply v=1 ok ", Body, Line),
    parse_frame(Line, Result),
    assertion(Result == ok(reply(1, ok(Term)))).

error_case("unknown-command", "unknown-command", none).
error_case("attack-cooldown", "attack-cooldown", none).
error_case("stale-attack last_attack_seq=17", "stale-attack", last_attack_seq(17)).
error_case("attack-seq-conflict last_attack_seq=17", "attack-seq-conflict", last_attack_seq(17)).
error_case("stale-repair last_repair_seq=4", "stale-repair", last_repair_seq(4)).
error_case("repair-seq-conflict last_repair_seq=4", "repair-seq-conflict", last_repair_seq(4)).

test(server_error, [forall(error_case(Body, Code, Detail))]) :-
    string_concat("reply v=1 error code=", Body, Line),
    parse_frame(Line, Result),
    assertion(Result == ok(reply(1, error(Code, Detail)))).

test(too_early) :-
    uuid(Id),
    format(string(Line),
           'reply v=1 error code=too-early operation_id=~s attempt_id=~s duration_ms=3000 elapsed_ms=250',
           [Id, Id]),
    parse_frame(Line, Result),
    assertion(Result == ok(reply(1, error("too-early", progress(Id, Id, 3000, 250))))).

test(event) :-
    parse_frame("event v=1 version=42 seq=42 changes=[gather-finished,resource-removed] resync=true",
                Result),
    assertion(Result == ok(event(1, 42, 42, ["gather-finished", "resource-removed"], true))).

test(empty_event_list) :-
    parse_frame("event v=1 version=0 seq=0 changes=[] resync=false", Result),
    assertion(Result == ok(event(1, 0, 0, [], false))).

empty_fields(" width=20 height=10 self=1 players=[] objects=[] total_wood=0 total_stone=0 blue=0 red=0 phase=active winner=none inventory_wood=0 inventory_stone=0 inventory_axe=0 inventory_pickaxe=0 attack_seq=0 repair_seq=0").
empty_world(world(size(20, 10), 1, [], [], totals(0, 0), score(0, 0),
                  match(active, none), inventory(0, 0, 0, 0), sequences(0, 0))).

test(empty_world) :-
    empty_fields(Fields),
    empty_world(World),
    string_concat("reply v=1 ok world", Fields, Line),
    parse_frame(Line, Result),
    assertion(Result == ok(reply(1, ok(World)))).

test(empty_versioned_world) :-
    empty_fields(Fields),
    empty_world(World),
    format(string(Line), 'reply v=1 ok world-versioned version=0~s online=[] gathering=[]',
           [Fields]),
    parse_frame(Line, Result),
    assertion(Result == ok(reply(1, ok(world_versioned(0, World, [], []))))).

world_line(Name, Line) :-
    uuid(Id),
    atomics_to_string([
        "reply v=1 ok world-versioned version=42 width=20 height=10 self=9007199254740993",
        " players=[player(2,2,0,red,\"Борис\"),player(9007199254740993,0,1,blue,\"",
        Name, "\")] objects=[obstacle(0,3,house),resource(4,5,tree),resource(6,8,stone),",
        "building(12,8,", Id, ",fence,red,2,40,50),building(13,8,none,house,none,none,none,none)]",
        " total_wood=5 total_stone=7 blue=0 red=100 phase=finished winner=red",
        " inventory_wood=3 inventory_stone=2 inventory_axe=1 inventory_pickaxe=0",
        " attack_seq=17 repair_seq=4 online=[2,9007199254740993] gathering=[2]"
    ], Line).

test(full_world_with_cyrillic_uuid_and_escapes) :-
    uuid(Id),
    world_line("Анна, []() \\\"Я\\\"\\\\\\u0000\\u000a\\u000d\\u0009\\u0085\\u2028\\u2029 😀", Line),
    string_codes(Name, [1040,1085,1085,1072,44,32,91,93,40,41,32,34,1071,34,92,
                        0,10,13,9,0x85,0x2028,0x2029,32,0x1f600]),
    World = world(size(20, 10), 9007199254740993,
                  [player(2, 2, 0, red, "Борис"),
                   player(9007199254740993, 0, 1, blue, Name)],
                  [obstacle(0, 3, "house"), resource(4, 5, tree), resource(6, 8, stone),
                   building(Id, 12, 8, "fence", red, 2, 40, 50),
                   building(none, 13, 8, "house", none, none, none, none)],
                  totals(5, 7), score(0, 100), match(finished, red),
                  inventory(3, 2, 1, 0), sequences(17, 4)),
    parse_frame(Line, Result),
    assertion(Result == ok(reply(1, ok(world_versioned(42, World, [2, 9007199254740993], [2]))))).

test(unsupported_version, [forall(member(Prefix-Version, ["reply"-"2", "event"-"11", "reply"-"01"]))]) :-
    format(string(Line), '~s v=~s payload', [Prefix, Version]),
    parse_frame(Line, Result),
    assertion(Result == error(unsupported_version(Version))).

invalid("").
invalid("reply v=1x ok move x=1 y=2").
invalid("reply v=1 ok unknown").
invalid("reply v=1 ok move y=2 x=1").
invalid("reply v=1 ok move x=1").
invalid("reply v=1 ok move x=1 y=2 x=3").
invalid("reply v=1 ok move x=-1 y=2").
invalid("reply v=1 ok move x=1 y=٢").
invalid("reply v=1 ok move x=1  y=2").
invalid("reply v=1 ok move x=1 y=2 ").
invalid("reply v=1 ok world width=20").
invalid("reply v=1 error code=stale-attack").
invalid("reply v=1 error code=too-early").
invalid("reply v=1 error code=stale-repair last_attack_seq=1").
invalid("reply v=1 error code=unknown-command message=extra").
invalid("reply v=1 ok gather-cancel operation_id=01234567-89Ab-cdef-0123-456789abcdef attempt_id=none").
invalid("reply v=1 ok gather-cancel operation_id=01234567-89ab-cdef-0123-456789abcde attempt_id=none").
invalid("reply v=1 ok build building=building(12,8,none,fence,green,2,40,50)").
invalid("reply v=1 ok build building=building(12,8,none,fence,red,2,40)").
invalid("event v=1 version=42 seq=41 changes=[] resync=true").
invalid("event v=1 version=42 seq=42 changes=[a,] resync=true").
invalid("event v=1 version=42 seq=42 changes=[] resync=yes").
invalid("event v=1 version=42 seq=42 changes=[]").
invalid(Line) :-
    member(Name, ["\\q", "\\u00xz", "\\u123", "\\ud800", "сырой\nперенос", "кавычка\""]),
    world_line(Name, Line).

test(invalid_frame, [forall(invalid(Line))]) :-
    parse_frame(Line, Result),
    assertion(Result == error(invalid_frame)).

test(invalid_input) :-
    parse_frame(end_of_file, Result),
    assertion(Result == error(invalid_input)).

:- end_tests(settlement_dcg).
