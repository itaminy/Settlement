:- module(settlement_knowledge,
          [ load_snapshot/1, apply_event/1, reset_knowledge/0,
            player/5, resource/3, known_resource/3, building/8, obstacle/3, world_size/2,
            score/2, inventory_item/2, world_version/1, match_state/2,
            bot_identity/2, world_totals/2, command_sequences/2,
            online_player/1, gathering_player/1,
            bot_position/2, nearest_resource/4, allied_building/1,
            enemy_building/1, resource_counts/2, current_score/2, needs_resync/0
          ]).

:- use_module(library(pcre)).

:- dynamic player/5, resource/3, known_resource/3, building/8, obstacle/3, world_size/2.
:- dynamic score/2, inventory_item/2, world_version/1, match_state/2.
:- dynamic bot_identity/2, world_totals/2, command_sequences/2.
:- dynamic online_player/1, gathering_player/1.
:- dynamic event_cursor/1, resync_pending/0.

% The module stores one active session. Reset at a connection boundary before
% passing frames from the new session. Use snapshot/1 for groups of reads.
reset_knowledge :-
    with_mutex(settlement_knowledge, transaction((
        clear_knowledge,
        retractall(known_resource(_, _, _))
    ))).

% Accepts the Frame inside parse_frame/2's ok(Frame). Invalid or outdated
% snapshots fail without changing any facts. A plain world has no version
% and cannot replace a versioned snapshot for the same bot.
load_snapshot(Frame) :-
    ground(Frame),
    once(snapshot_facts(Frame, Version, Self, Facts)),
    with_mutex(settlement_knowledge, transaction((
        fresh_snapshot(Self, Version),
        ( bot_identity(Self, _) -> true ; retractall(known_resource(_, _, _)) ),
        clear_knowledge,
        maplist(assertz, Facts),
        forall(resource(X, Y, Type), remember_resource(X, Y, Type))
    ))).

% A harvested resource can return after it disappears from a live snapshot.
remember_resource(X, Y, Type) :-
    ( known_resource(X, Y, Type) -> true ; assertz(known_resource(X, Y, Type)) ).

snapshot_facts(reply(1, ok(world_versioned(Version, World, Online, Gathering))),
               Version, Self, Facts) :-
    natural(Version),
    valid_ids(Online),
    valid_ids(Gathering),
    maplist(online_fact, Online, OnlineFacts),
    maplist(gathering_fact, Gathering, GatheringFacts),
    world_facts(World, Self, WorldFacts),
    append([[world_version(Version), event_cursor(Version)],
            OnlineFacts, GatheringFacts, WorldFacts], Facts).
snapshot_facts(reply(1, ok(World)), none, Self, Facts) :-
    world_facts(World, Self, Facts).

world_facts(world(size(Width, Height), Self, Players, Objects, totals(TotalWood, TotalStone),
                  score(Blue, Red), match(Phase, Winner), inventory(Wood, Stone, Axe, Pickaxe),
                  sequences(Attack, Repair)), Self, Facts) :-
    positive(Width), positive(Height),
    is_list(Players), is_list(Objects),
    maplist(player_key(Width, Height), Players, PlayerIds),
    unique(PlayerIds),
    memberchk(player(Self, _, _, Team, _), Players),
    maplist(object_key(Width, Height), Objects, Cells, BuildingIds),
    unique(Cells),
    exclude(==(none), BuildingIds, UUIDs),
    unique(UUIDs),
    maplist(natural, [TotalWood, TotalStone, Blue, Red, Wood, Stone, Axe, Pickaxe, Attack, Repair]),
    memberchk(Phase, [active, finished]),
    memberchk(Winner, [none, blue, red]),
    append([Players, Objects,
            [world_size(Width, Height), bot_identity(Self, Team),
             world_totals(TotalWood, TotalStone), command_sequences(Attack, Repair),
             score(blue, Blue), score(red, Red), match_state(Phase, Winner),
             inventory_item(wood, Wood), inventory_item(stone, Stone),
             inventory_item(axe, Axe), inventory_item(pickaxe, Pickaxe)]], Facts).

player_key(Width, Height, player(Id, X, Y, Team, Name), Id) :-
    positive(Id), cell(Width, Height, X, Y), team(Team), string(Name).

object_key(Width, Height, resource(X, Y, Type), X-Y, none) :-
    cell(Width, Height, X, Y), memberchk(Type, [tree, stone]).
object_key(Width, Height, obstacle(X, Y, Kind), X-Y, none) :-
    cell(Width, Height, X, Y), token(Kind).
object_key(Width, Height, building(Id, X, Y, Kind, Team, Owner, HP, MaxHP), X-Y, Id) :-
    cell(Width, Height, X, Y), token(Kind), optional(uuid, Id),
    optional(team, Team), optional(natural, Owner),
    optional(natural, HP), optional(natural, MaxHP).

cell(Width, Height, X, Y) :-
    natural(X), natural(Y), X < Width, Y < Height.

valid_ids(Ids) :-
    is_list(Ids), maplist(positive, Ids), unique(Ids).

unique(Values) :-
    sort(Values, Sorted), same_length(Values, Sorted).

natural(Value) :- integer(Value), Value >= 0.
positive(Value) :- integer(Value), Value > 0.
team(Team) :- memberchk(Team, [blue, red]).
optional(_, none) :- !.
optional(Check, Value) :- call(Check, Value).

token(Value) :- string(Value), re_match('^[a-z0-9-]+\\z', Value).
uuid(Value) :-
    string(Value),
    re_match('^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}\\z', Value).

online_fact(Id, online_player(Id)).
gathering_fact(Id, gathering_player(Id)).

fresh_snapshot(Self, Version) :-
    ( bot_identity(Self, _), event_cursor(Last) ->
        integer(Version), Version >= Last
    ; true
    ).

clear_knowledge :-
    retractall(player(_, _, _, _, _)),
    retractall(resource(_, _, _)),
    retractall(building(_, _, _, _, _, _, _, _)),
    retractall(obstacle(_, _, _)),
    retractall(world_size(_, _)),
    retractall(score(_, _)),
    retractall(inventory_item(_, _)),
    retractall(world_version(_)),
    retractall(match_state(_, _)),
    retractall(bot_identity(_, _)),
    retractall(world_totals(_, _)),
    retractall(command_sequences(_, _)),
    retractall(online_player(_)),
    retractall(gathering_player(_)),
    retractall(event_cursor(_)),
    retractall(resync_pending).

% Events update synchronization bookkeeping only. world_version/1 is the
% version of the facts in the last full snapshot.
apply_event(Frame) :-
    ground(Frame),
    Frame = event(1, Version, Seq, Changes, Resync),
    natural(Version), natural(Seq),
    is_list(Changes), maplist(token, Changes),
    memberchk(Resync, [true, false]),
    with_mutex(settlement_knowledge, transaction(record_event(Version, Seq, Resync))).

record_event(Version, Seq, Resync) :-
    ( Version =\= Seq ->
        mark_resync
    ; world_version(Base) ->
        ( Seq =< Base -> true
        ; event_cursor(Last),
          ( Seq =:= Last + 1, Resync == false -> true ; mark_resync ),
          ( Seq > Last ->
              retractall(event_cursor(_)),
              assertz(event_cursor(Seq))
          ; true
          )
        )
    ; mark_resync
    ).

mark_resync :-
    ( resync_pending -> true ; assertz(resync_pending) ).

needs_resync :-
    snapshot(( resync_pending -> true ; \+ world_version(_) )).

bot_position(X, Y) :-
    snapshot((bot_identity(Id, Team), player(Id, X, Y, Team, _))).

% Type is tree or stone (or unbound for either). Manhattan distance assumes
% four-direction movement. Ties use X, then Y. Fails with no matching resource.
nearest_resource(Type, X, Y, Distance) :-
    snapshot((
        bot_identity(Id, Team),
        player(Id, BotX, BotY, Team, _),
        setof(candidate(D, RX, RY, Kind),
              ( resource(RX, RY, Kind),
                (var(Type) -> true ; Type == Kind),
                D is abs(RX - BotX) + abs(RY - BotY)
              ),
              [candidate(Nearest, NearX, NearY, NearKind) | _])
    )),
    Type = NearKind, X = NearX, Y = NearY, Distance = Nearest.

allied_building(Building) :-
    snapshot(findall(Building, team_building(allied, Building), Buildings)),
    member(Building, Buildings).

enemy_building(Building) :-
    snapshot(findall(Building, team_building(enemy, Building), Buildings)),
    member(Building, Buildings).

team_building(Relation, building(Id, X, Y, Kind, Team, Owner, HP, MaxHP)) :-
    bot_identity(_, BotTeam),
    building(Id, X, Y, Kind, Team, Owner, HP, MaxHP),
    team(Team),
    ( Relation == allied -> BotTeam == Team ; BotTeam \== Team ).

resource_counts(Wood, Stone) :-
    snapshot((inventory_item(wood, Wood), inventory_item(stone, Stone))).

current_score(Blue, Red) :-
    snapshot((score(blue, Blue), score(red, Red))).
