# Лабораторная работа №1

`Степанов Арсений Алексеевич - 368849`

## Задачи

Освоить базовые приёмы и абстракции функционального программирования:
функции, поток управления и поток данных, сопоставление с образцом, рекурсия,
свёртка, отображение, работа с функциями как с данными, списки

В рамках лабораторной работы вам предлагается решить несколько задач
[проекта Эйлер](https://projecteuler.net/archives)

Для каждой проблемы должно быть представлено несколько решений:

1. Монолитные реализации с использованием:
   - Хвостовой рекурсии
   - Рекурсии (вариант с хвостовой рекурсией не является примером рекурсии)
2. Модульной реализации, где явно разделена генерация последовательности,
   фильтрация и свёртка (должны использоваться функции `reduce`/`fold`,
   `filter` и аналогичные)
3. Генерация последовательности при помощи отображения (`map`)
4. Работа со специальным синтаксисом для циклов
5. Работа с бесконечными списками для языков, поддерживающих ленивые коллекции
   или итераторы как часть языка
6. Реализация на любом удобном для вас традиционном языке
   программирования для сравнения

Требуется использовать идиоматичный для технологии стиль программирования

## Задача проекта Эйлера №5

Найдите **наименьшее положительное число**, которое делится на все числа
**от 1 до 20** без остатка. Иными словами, требуется найти **наименьшее общее
кратное** чисел $1, 2, 3, \cdots, 20$

### Реализации (Задача №5)

#### Реализация на Julia (Задача №5)

```julia
function prob5(n)
    result = 1
    for i in 1:n
        result = lcm(result, i)
    end
    return result
end
```

#### Рекурсивная реализация (Задача №5)

```haskell
recProb5 :: Int -> Int
recProb5 x | x <= 1    = 1
           | otherwise = lcm' x (recProb5 (x - 1))
 where
  gcd' a b | b == 0    = a
           | otherwise = gcd' b (a `mod` b)
  lcm' a b = abs (a * b) `div` gcd' a b
```

#### Реализация через хвостовую рекурсию (Задача №5)

```haskell
tailRecProb5 :: Int -> Int
tailRecProb5 x = aux x 1
 where
  aux n acc | n <= 1    = acc
            | otherwise = aux (n - 1) (lcm' n acc)
  gcd' a b | b == 0    = a
           | otherwise = gcd' b (a `mod` b)
  lcm' a b = abs (a * b) `div` gcd' a b
```

#### Реализация с использованием бесконечных списков (Задача №5)

```haskell
infiniteListProb5 :: Int -> Int
infiniteListProb5 x = last $ take x (aux 1 1)
 where
  aux a acc = acc : aux (a + 1) (lcm' a acc)
  gcd' a b | b == 0    = a
           | otherwise = gcd' b (a `mod` b)
  lcm' a b = abs (a * b) `div` gcd' a b
```

#### Модульная реализация (Задача №5)

```haskell
moduleProb5 :: Int -> Int
moduleProb5 x | x <= 1    = 1
              | otherwise = foldr1 lcm' [1 .. x]
 where
  gcd' a b | b == 0    = a
           | otherwise = gcd' b (a `mod` b)
  lcm' a b = abs (a * b) `div` gcd' a b
```

## Задача проекта Эйлера №25

Найдите **первый член последовательности Фибоначчи**, который содержит
**1000 цифр**

Последовательность Фибоначчи определяется как:

$$
F_1 = 1, \quad F_2 = 1, \quad F_n = F_{n-1} + F_{n-2} \quad \text{для } n \ge 3
$$

Необходимо определить **номер члена последовательности** $n$, у которого
количество цифр достигает 1000

### Реализации (Задача №25)

#### Реализация на Julia (Задача №25)

```julia
function prob25(ndigits)
    a, b = BigInt(1), BigInt(1)
    index = 2
    while length(string(b)) < ndigits
        a, b = b, a + b
        index += 1
    end
    return index
end
```

#### Рекурсивная реализация (Задача №25)

```haskell
recProb25 :: Int -> Int
recProb25 x =
  let initial = 2
      limit   = 10 ^ (x - 1)
      aux :: Integer -> Integer -> Int
      aux a b | a + b >= limit = 1
              | otherwise      = 1 + aux b (a + b)
  in  initial + aux 1 1
```

#### Реализация через хвостовую рекурсию (Задача №25)

```haskell
tailRecProb25 :: Int -> Int
tailRecProb25 x =
  let initial = 2
      limit   = 10 ^ (x - 1)
      aux :: Integer -> Integer -> Int -> Int
      aux a b acc | a + b >= limit = acc + 1
                  | otherwise      = aux b (a + b) (acc + 1)
  in  initial + aux 1 1 0
```

#### Реализация с использованием бесконечных списков (Задача №25)

```haskell
infiniteListProb25 :: Int -> Int
infiniteListProb25 x = (+) 1 $ length $ takeWhile (< limit) fibs
 where
  limit = 10 ^ (x - 1)
  fibs :: [Integer]
  fibs = 1 : 1 : zipWith (+) fibs (drop 1 fibs)
```

#### Модульная реализация (Задача №25)

```haskell
moduleProb25 :: Int -> Int
moduleProb25 x = fst $ foldr1 const $ filter (\(_, val) -> val > limit) $ zip
  [1 ..]
  fibs
 where
  limit :: Integer
  limit = 10 ^ (x - 1)
  fibs :: [Integer]
  fibs = 1 : 1 : zipWith (+) fibs (drop 1 fibs)
```

## Тесты

Для тестирования был использован тестовый фреймворк `Tasty` для Haskell

```haskell
import           Test.Tasty
import           Test.Tasty.HUnit
import           Prob5
import           Prob25

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup
  "Lab 1 tests"
  [ testGroup
    "Problem 5 variants"
    [ testCase "recProb5 10" $ recProb5 10 @?= 2520
    , testCase "tailRecProb5 10" $ tailRecProb5 10 @?= 2520
    , testCase "mapProb5 10" $ mapProb5 10 @?= 2520
    , testCase "infiniteListProb5 10" $ infiniteListProb5 10 @?= 2520
    , testCase "moduleProb5 10" $ moduleProb5 10 @?= 2520
    , testCase "recProb5 20" $ recProb5 20 @?= 232792560
    , testCase "tailRecProb5 20" $ tailRecProb5 20 @?= 232792560
    , testCase "mapProb5 20" $ mapProb5 20 @?= 232792560
    , testCase "infiniteListProb5 20" $ infiniteListProb5 20 @?= 232792560
    , testCase "moduleProb5 20" $ moduleProb5 20 @?= 232792560
    , testCase "All variants (n=10) equal"
      $ let n = 10
            vals =
              [tailRecProb5 n, mapProb5 n, infiniteListProb5 n, moduleProb5 n]
        in  all (== recProb5 n) vals @? "Mismatch between implementations"
    , testCase "All variants (n=20) equal"
      $ let n = 20
            vals =
              [tailRecProb5 n, mapProb5 n, infiniteListProb5 n, moduleProb5 n]
        in  all (== recProb5 n) vals @? "Mismatch between implementations"
    ]
  , testGroup
    "Problem 25 variants"
    [ testCase "recProb25 3" $ recProb25 3 @?= 12
    , testCase "tailRecProb25 3" $ tailRecProb25 3 @?= 12
    , testCase "mapProb25 3" $ mapProb25 3 @?= 12
    , testCase "infiniteListProb25 3" $ infiniteListProb25 3 @?= 12
    , testCase "moduleProb25 3" $ moduleProb25 3 @?= 12
    , testCase "recProb25 1000" $ recProb25 1000 @?= 4782
    , testCase "tailRecProb25 1000" $ tailRecProb25 1000 @?= 4782
    , testCase "mapProb25 1000" $ mapProb25 1000 @?= 4782
    , testCase "infiniteListProb25 1000" $ infiniteListProb25 1000 @?= 4782
    , testCase "moduleProb25 1000" $ moduleProb25 1000 @?= 4782
    , testCase "All variants (n=3) equal"
      $ let
          n = 3
          vals =
            [tailRecProb25 n, mapProb25 n, infiniteListProb25 n, moduleProb25 n]
        in
          all (== recProb25 n) vals @? "Mismatch between implementations"
    , testCase "All variants (n=1000) equal"
      $ let
          n = 1000
          vals =
            [tailRecProb25 n, mapProb25 n, infiniteListProb25 n, moduleProb25 n]
        in
          all (== recProb25 n) vals @? "Mismatch between implementations"
    ]
  ]
```

## Выводы

В ходе выполнения лабораторной работы были изучены следующие базовые приёмы и
абстракции функционального программирования:

- **Функции**
- **Поток управления и поток данных**
- **Сопоставление с образцом**
- **Рекурсия**
- **Свёртка**
- **Отображение**
- **Работа с функциями как с данными**
- **Списки**

Были решены задачи проекта Эйлера:

1. **Задача №5**: Найти наименьшее положительное число, которое делится на
   все числа от 1 до 20 без остатка
2. **Задача №25**: Найти первый член последовательности Фибоначчи,
   который содержит 1000 цифр

Для каждой задачи были представлены несколько решений:

- **Монолитные реализации**:
  - С использованием рекурсии
  - С использованием хвостовой рекурсии
- **Модульные реализации**:
  - С разделением генерации последовательности, фильтрации и свёртки
- **Реализации с использованием отображения (`map`)**
- **Реализации с использованием бесконечных списков**

Все решения были выполнены в идиоматичном стиле программирования
для языка **Haskell**
