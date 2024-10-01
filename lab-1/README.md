# Лабораторная работа №1

Цель: освоить базовые приёмы и абстракции функционального программирования:
функции, поток управления и поток данных, сопоставление с образцом, рекурсия,
свёртка, отображение, работа с функциями как с данными, списки.
В рамках лабораторной работы вам предлагается решить несколько задач проекта
Эйлер. Список задач -- ваш вариант.
Для каждой проблемы должно быть представлено несколько решений:

1. монолитные реализации с использованием:

- хвостовой рекурсии
- рекурсии (вариант с хвостовой рекурсией не является примером рекурсии)

2. модульной реализации, где явно разделена генерация последовательности,
   фильтрация и свёртка (должны использоваться функции reduce/fold, filter и
   аналогичные);
3. генерация последовательности при помощи отображения (map);
4. работа со спец. синтаксисом для циклов (где применимо);
5. работа с бесконечными списками для языков, поддерживающих ленивые коллекции
   или итераторы как часть языка (к примеру Haskell, Clojure);
6. реализация на любом удобном для вас традиционном языке программирования для
   сравнения.

Требуется использовать идиоматичный для технологии стиль программирования.
Содержание отчёта:

- титульный лист;
- описание проблемы;
- ключевые элементы реализации с минимальными комментариями;
- выводы (отзыв об использованных приёмах программирования).

Примечания:

- необходимо понимание разницы между ленивыми коллекциями и итераторами;
- нужно знать особенности используемой технологии и того, как работают
  использованные вами приёмы.

## Задача проекта Эйлера №3

Найти наибольший простой делитель числа 600851475143

### Реализации

#### Реализация на C++

```cpp
#include <iostream>

auto largest_prime_factor(uint64_t n) -> uint64_t {
    uint64_t largest_factor = 1;
    uint64_t factor = 2;

    while (factor * factor <= n) {
        if (n % factor == 0) {
            largest_factor = factor;
            while (n % factor == 0) {
                n /= factor;
            }d
        }
        factor = (factor == 2) ? 3 : factor + 2;
    }

    if (n > 1) {
        largest_factor = n;
    }

    return largest_factor;
}
```

#### *Традиционная* реализация

```rust
pub(crate) fn solution(n: u64) -> u64 {
    let mut n = n;
    let mut factor = 2;
    while n > 1 {
        if n % factor == 0 {
            n /= factor;
        } else {
            factor += 1;
        }
    }
    factor
}
```

#### Рекурсивная реализация

```rust
pub(crate) fn recursive_solution(n: u64) -> u64 {
    fn recursive_solution(n: u64, factor: u64) -> u64 {
        if n == 1 {
            factor
        } else if n % factor == 0 {
            recursive_solution(n / factor, factor)
        } else {
            recursive_solution(n, factor + 1)
        }
    }
    recursive_solution(n, 2)
}
```

#### Реализация с использованием map

```rust
pub(crate) fn map_solution(n: u64) -> u64 {
    (2..(n as f64).sqrt() as u64 + 1)
        .filter(|&x| n % x == 0)
        .filter(|&x| (2..(x as f64).sqrt() as u64 + 1).all(|y| x % y != 0))
        .last()
        .unwrap()
}
```

#### Реализация с использованием ленивых вычислений

```rust
fn prime_numbers() -> impl Iterator<Item=u64> {
    (1..)
        .filter(|&x| (2..(x as f64).sqrt() as u64 + 1).all(|y| x % y != 0))
}

pub(crate) fn infinite_list_solution(n: u64) -> u64 {
    prime_numbers()
        .take_while(|&x| x <= n)
        .filter(|&x| n % x == 0)
        .last()
        .unwrap()
}
```

## Задача проекта Эйлера №28

Начав с числа 1 и двигаясь вправо по часовой стрелке,
образуется спираль 5 на 5 следующим образом:

```text
21 22 23 24 25
20  7  8  9 10
19  6  1  2 11
18  5  4  3 12
17 16 15 14 13
```

Можно проверить, что сумма чисел по диагоналям равна 101.

Какова сумма чисел по диагоналям в спирали размером 1001 на 1001, образованной
аналогичным способом?

### Реализации

#### Реализация на C++

```cpp
auto spiral_diagonals_sum(uint64_t size) -> uint64_t {
    uint64_t sum = 1; // Начальное число в центре спирали
    uint64_t current_number = 1;

    for (size_t layer = 1; layer <= size / 2; ++layer) {
        uint64_t step = layer * 2;
        for (uint64_t corner = 0; corner < 4; ++corner) {
            current_number += step;
            sum += current_number;
        }
    }

    return sum;
}
```

#### *Традиционная* реализация

```rust
pub(crate) fn solution(n: u64) -> u64 {
    let mut n = n;
    let mut factor = 2;
    while n > 1 {
        if n % factor == 0 {
            n /= factor;
        } else {
            factor += 1;
        }
    }
    factor
}
```

#### Рекурсивная реализация

```rust
pub(crate) fn recursive_solution(n: u64) -> u64 {
    if n == 1 {
        1
    } else {
        solution(n - 2) + 4 * n * n - 6 * (n - 1)
    }
}
```

#### Реализация с использованием map

```rust
pub(crate) fn map_solution(n: u64) -> u64 {
    (2..=n)
        .step_by(2)
        .flat_map(|x| std::iter::repeat(x).take(4))
        .scan(1, |acc, curr| {
            *acc += curr;
            Some(*acc)
        })
        .fold(1, |acc, x| acc + x)
}
```

#### Реализация с использованием ленивых вычислений

```rust
fn spiral_diagonals() -> impl Iterator<Item=u64> {
    (2..)
        .step_by(2)
        .flat_map(|x| std::iter::repeat(x).take(4))
}

pub(crate) fn infinite_list_solution(n: u64) -> u64 {
    spiral_diagonals()
        .take(((n / 2) * 4) as usize)
        .scan(1, |acc, curr| {
            *acc += curr;
            Some(*acc)
        })
        .fold(1, |acc, x| acc + x)
}
```

## Тесты

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn prob3_pre() {
        let number = 13195;
        let expected = 29;
        let result = prob3::solution(number);
        assert_eq!(result, expected);
        let result = prob3::recursive_solution(number);
        assert_eq!(result, expected);
        let result = prob3::map_solution(number);
        assert_eq!(result, expected);
        let result = prob3::infinite_list_solution(number);
        assert_eq!(result, expected);
    }

    #[test]
    fn prob3() {
        let number = 600851475143;
        let expected = 6857;
        let result = prob3::solution(number);
        assert_eq!(result, expected);
        let result = prob3::recursive_solution(number);
        assert_eq!(result, expected);
        let result = prob3::map_solution(number);
        assert_eq!(result, expected);
    }

    #[test]
    fn prob28_pre() {
        let number = 5;
        let expected = 101;
        let result = prob28::solution(number);
        assert_eq!(result, expected);
        let result = prob28::recursive_solution(number);
        assert_eq!(result, expected);
        let result = prob28::map_solution(number);
        assert_eq!(result, expected);
        let result = prob28::infinite_list_solution(number);
        assert_eq!(result, expected);
    }

    #[test]
    fn prob28() {
        let number = 1001;
        let expected = 669171001;
        let result = prob28::solution(number);
        assert_eq!(result, expected);
        let result = prob28::recursive_solution(number);
        assert_eq!(result, expected);
        let result = prob28::map_solution(number);
        assert_eq!(result, expected);
        let result = prob28::infinite_list_solution(number);
        assert_eq!(result, expected);
    }
}
```

## Выводы

В ходе выполнения лабораторной работы были изучены базовые приёмы и абстракции
функционального программирования: функции, поток управления и поток данных,
сопоставление с образцом, рекурсия, свёртка, отображение, работа с функциями как
с данными, списки. Были решены задачи проекта Эйлера №3 и №28. Для каждой
проблемы были представлены несколько решений, включая монолитные и модульные
реализации, реализации с использованием отображения и с бесконечными списками.
Были использованы идиоматичные для Rust стили программирования.