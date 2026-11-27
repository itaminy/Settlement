:- encoding(utf8).
:- use_module(settlement_client).
:- use_module(settlement_dcg).
:- use_module(settlement_knowledge, except([resource/3])).
:- use_module(library(aggregate)).
:- initialization(main, main).

main(Args) :-
    ( Args = [IdText],
      catch(atom_number(IdText, Id), _, fail),
      integer(Id), Id > 0 ->
        catch(run(Id), Error, report_failure(Error))
    ; format(user_error, 'Укажите свободный положительный ID игрока.~n', []),
      halt(2)
    ).

run(Id) :-
    server_port(Port),
    format('Подключение к Settlement на 127.0.0.1:~d.~n', [Port]),
    Name = "Бот SWI-Prolog",
    reset_knowledge,
    fetch_snapshot(Id, register(Name), Credentials, FirstSnapshot),
    store_snapshot(FirstSnapshot, Id, Name, Team),
    format('Регистрация выполнена, текстовый режим включён.~n', []),
    print_knowledge,
    format('Первое соединение закрыто. Проверяю повторный вход.~n', []),
    reset_knowledge,
    fetch_snapshot(Id, login(Credentials), _, Snapshot),
    store_snapshot(Snapshot, Id, Name, Team),
    format('Повторный вход выполнен, соединение закрыто.~n', []),
    print_knowledge.

store_snapshot(Line, Id, Name, Team) :-
    parse_frame(Line, Result),
    ( Result = ok(Frame),
      Frame = reply(1, ok(world_versioned(Version, World, _, _))),
      World = world(_, Id, Players, _, _, _, _, _, _),
      memberchk(player(Id, X, Y, Team, Name), Players),
      load_snapshot(Frame),
      world_version(Version),
      online_player(Id),
      bot_identity(Id, Team),
      bot_position(X, Y),
      \+ needs_resync ->
        true
    ; Result = error(Reason) ->
        throw(settlement_error(snapshot_parse_error(Reason)))
    ; throw(settlement_error(unexpected_parsed_snapshot))
    ).

print_knowledge :-
    world_version(Version),
    world_size(Width, Height),
    bot_identity(Id, Team),
    bot_position(X, Y),
    aggregate_all(count, player(_, _, _, _, _), Players),
    aggregate_all(count, settlement_knowledge:resource(_, _, _), Resources),
    aggregate_all(count, building(_, _, _, _, _, _, _, _), Buildings),
    aggregate_all(count, obstacle(_, _, _), Obstacles),
    aggregate_all(count, bandit(_, _, _, _, _, _, _), Bandits),
    resource_counts(Wood, Stone),
    current_score(Blue, Red),
    match_state(Phase, Winner),
    format('База фактов, версия ~d, карта ~dx~d. Бот ~d, команда ~w, позиция (~d, ~d).~n',
           [Version, Width, Height, Id, Team, X, Y]),
    format('Игроков ~d, ресурсов ~d, построек ~d, препятствий ~d.~n',
           [Players, Resources, Buildings, Obstacles]),
    format('Бандитов в снимке ~d.~n', [Bandits]),
    format('Инвентарь (~d дерева, ~d камня), счёт (~d синие, ~d красные), матч (~w, ~w).~n',
           [Wood, Stone, Blue, Red, Phase, Winner]).

report_failure(settlement_error(rejected(Phase, Code))) :-
    !,
    format(user_error, 'Сервер отклонил запрос (~w, ~s).~n', [Phase, Code]),
    halt(1).
report_failure(settlement_error(stale_server)) :-
    !,
    format(user_error, 'Сервер перезапустился. Токен предыдущего экземпляра недействителен.~n', []),
    halt(1).
report_failure(settlement_error(Reason)) :-
    !,
    format(user_error, 'Обмен с сервером прерван (~w).~n', [Reason]),
    halt(1).
report_failure(_) :-
    format(user_error, 'Ошибка TCP или локального ввода-вывода. Проверьте сервер и настройку SETTLEMENT_PORT.~n', []),
    halt(1).
