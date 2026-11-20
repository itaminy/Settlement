# Settlement

Многопользовательская игра на Clojure. Автономный игрок на SWI-Prolog
подключается к тому же TCP-серверу.

Запуск сервера из корня репозитория:

~~~powershell
clojure -M:run
~~~

Полный набор тестов Clojure:

~~~powershell
clojure -M -m settlement.world-test all
~~~

- [Запуск и тестирование Prolog-бота](prolog/README.md)
- [Текстовый протокол TCP, версия 1](docs/text-protocol.md)
