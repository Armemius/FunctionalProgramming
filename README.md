# Обоснование выбора языка

Мне давно была интересна тема различных парадигм программирования, чаще и больше
всего изучаются процедурные и
объектно-ориентированные подходы, но в современных реалиях только ими не
ограничивается, всё большее распространение
получает функциональная парадигма. Большое впечатление на меня, как человека,
который много занимается низкоуровневой
разработкой на C и C++, произвёл Rust. Благодаря ему я узнал про новые и более
лаконичные способы решения задач с
использованием пресловутой функциональной парадигмы, над решением которых раньше
приходилось что-то изобретать.

После первого знакомства с парадигмой и выбора чисто функционального языка для
дальнейшего изучения я пришёл к
следующему списку того, что мне интересно для изучения в рамках данного курса:

- **Rust** -- за возможность заниматься низкоуровневой разработкой применяя
  приёмы функционального программирования,
  надёжность, скорость и безопасность
- **OCaml** -- за гибкость между императивным и функциональным стилем
  программирования
- **Haskell** -- за чистую функциональность языка, его лаконичность, строгость и
  способность создавать абстракции
  высокого уровня без потери производительности

Несмотря на то что **Rust** не рекомендовался для изучения в рамках данного
курса, я всё-таки хотел бы использовать
именно его для выполнения работ. На проектах на моей работе потихоньку начинают
переходить на **Rust** с **C** и **C++**
и данный курс может помочь с углублением в функциональную часть **Rust**'а как
дополнение к основному изучению
особенностей языка, которые мне необходимо будет освоить в любом случае.

В качестве проекта я хотел бы отойти от системного программирования, которым
много занимался в последнее время и
познакомиться с разработкой бэкенда, изучить возможные библиотеки. Я рассчитываю
на то что этот опыт может пригодиться
при работе с сетями при разработке программного обеспечения для встроенных
систем

## Материалы для изучения

В качестве основного материала для изучения предлагается использовать
официальную документацию, собранную в одной книге.
Она доступна бесплатно и представляет исчерпывающее описание синтаксиса,
семантики и механизмов языка

[Ссылка на книгу в веб-формате](https://doc.rust-lang.org/stable/book/)

## Используемый инструментарий

- Компилятор:

  - Rustc — это стандартный компилятор Rust, который отвечает за
    преобразование кода в машинный. Он строго
    типизирован, что позволяет находить ошибки на этапе компиляции,
    обеспечивая безопасность при работе с памятью без
    необходимости использовать сборщик мусора.

- Система сборки:

  - Cargo — это встроенный менеджер пакетов и система сборки для проектов на
    Rust. Cargo автоматизирует такие задачи,
    как управление зависимостями, сборка проекта, запуск тестов и генерация
    документации. Важное преимущество Cargo —
    это интеграция с экосистемой Crates, которая позволяет легко использовать
    сторонние библиотеки.

- Инструменты для автоматического форматирования:

  - rustfmt — инструмент для автоматического форматирования кода в
    соответствии со стандартами оформления Rust. Это
    официальное средство форматирования, которое помогает поддерживать единый
    стиль во всех проектах.

- Lint tools:

  - Clippy — это популярный статический анализатор для Rust, который помогает
    выявлять неэффективный или потенциально
    опасный код. Clippy также даёт советы по улучшению стиля и качества кода.

- Инструменты тестирования:

  - Встроенные возможности тестирования с использованием аннотации `#[test]`
    позволяют писать модульные и
    интеграционные тесты. Cargo поддерживает запуск тестов через команду
    `cargo test`.
  - Proptest — библиотека для тестирования на основе свойств (property-based
    testing), которая позволяет находить
    пограничные случаи в коде, автоматически генерируя тестовые данные.

- Стиль кодирования:

  - Rust имеет официальный Rust Style Guide, который определяет правила
    оформления и рекомендации для написания
    читаемого и поддерживаемого кода. Поддерживается использование snake_case
    для имен переменных и функций, а также
    CamelCase для имен типов и структур.

## Источники

- [Официальный сайт Rust](https://rust-lang.org)
- [Документация по Rust](https://doc.rust-lang.org)
- [Официальный сайт Haskell](https://www.haskell.org/)
- [Статьи про Haskell на Habr](https://habr.com/ru/hubs/haskell/articles/)
- [Официальный сайт OCaml](https://ocaml.org/)
