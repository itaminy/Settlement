# Settlement

Многопользовательская игра на Clojure. Автономный игрок на SWI-Prolog
подключается к тому же TCP-серверу.

Запуск сервера из корня репозитория:

~~~powershell
clojure -M:run
~~~

Запуск Swing-клиента с главным меню:

~~~powershell
clojure -M:gui 1 "Ратмир"
~~~

Явный выбор графики World V7.0 и возврат к V6.5.2

~~~powershell
clojure '-J-Dsettlement.world.version=7.0' -M:gui 1 "Ратмир"
clojure '-J-Dsettlement.world.version=6.5.2' -M:gui 1 "Ратмир"
~~~

Атласы воды и суши World V7.0 подготовлены для seed 592321. Для другой карты
клиент использует её исходные слои, пока не подготовлены атласы с тем же seed.
Команды сборки, замеры и кадры Swing приведены в [состоянии проекта](docs/PROJECT_STATE.md#18-world-v70).

Полный набор тестов Clojure:

~~~powershell
clojure -M -m settlement.world-test all
~~~

В Swing удерживайте стрелки или WASD для перемещения. Управление отправляет
следующий шаг после ответа сервера и окончания текущей анимации.
Переменная `SETTLEMENT_MOVE_ANIMATION_MS` задаёт длительность визуального
шага в миллисекундах. По умолчанию клиент использует 220 мс.

- [Запуск и тестирование Prolog-бота](prolog/README.md)
- [Текстовый протокол TCP, версия 1](docs/text-protocol.md)
- [Интерфейс Swing, настройки и превью](docs/ui-v1.md)
