# Лабораторная работа №3

`Степанов Арсений - 368849`

## Возможности

- Потоковая обработка: программа читает точки `x y`
  (также поддерживается `x;y` или таб) со стандартного ввода и по мере
  поступления данных выводит интерполированные значения.
- Алгоритмы: линейная интерполяция отрезками и интерполяция Ньютона по
  скользящему окну фиксированного размера.
- Генератор точек: частота дискретизации задаётся параметром `--step`;
  один запуск может считать несколько алгоритмов одновременно.

## Запуск

Параметры CLI:

- `--linear` — включить линейную интерполяцию (по умолчанию используется,
  если ничего не выбрано).
- `--newton` — включить интерполяцию Ньютона.
- `--lagrange` — включить интерполяцию Лагранжа.
- `-n, --points N` — размер окна для Ньютона и Лагранжа (>= 2),
  по умолчанию `4`.
- `-s, --step STEP` — шаг по оси X для генерируемых точек.

Примеры:

```bash
# Линейная интерполяция с шагом 0.5
cat data.txt | stack run demo -- --linear --step 0.5

# Одновременный расчёт нескольких интерполяций с шагом 0.7
cat data.txt | stack run demo -- --linear --newton --points 4 --step 0.7
```

Входной поток должен быть отсортирован по возрастанию `x`. Программа
выводит строки вида:

```text
linear: 0 0
newton: 0.5 1.2
```

Если поступает точка с меньшим `x`, чем предыдущая,
программа завершается с ошибкой.

## Тесты

```bash
stack test
```

### Вывод тестов

```text
parsePoint
  parses semicolon separated pairs [✔]
  parses whitespace separated pairs [✔]
  rejects invalid input [✔]
linear interpolation
  interpolates a simple line with step 0.5 [✔]
Newton interpolation
  matches x^2 on four points with window 3 [✔]
Lagrange interpolation
  matches x^2 on four points with window 3 [✔]
streaming behaviour
  delays output until enough points arrive [✔]
  rejects decreasing x [✔]
properties
  preserves endpoints for linear interpolation [✔]
    +++ OK, passed 100 tests.
  is linear in the middle [✔]
    +++ OK, passed 100 tests.

Finished in 0.0128 seconds
10 examples, 0 failures
```
