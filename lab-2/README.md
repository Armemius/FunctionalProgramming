# Лабораторная работа №2

`Степанов Арсений Алексеевич - 368849`

Цель: освоиться с построением пользовательских типов данных, полиморфизмом,
рекурсивными алгоритмами и средствами тестирования (unit testing,
property-based testing), а также разделением интерфейса и особенностей
реализации.

В рамках лабораторной работы вам предлагается реализовать одну из предложенных
классических структур данных (список, дерево, бинарное дерево, hashmap,
граф...)

Текущий вариант: `avl-set`

Требования:

1. Функции:
    - добавление и удаление элементов
    - фильтрация
    - отображение (map)
    - свертки (левая и правая)
    - структура должна быть моноидом
2. Структуры данных должны быть неизменяемыми
3. Библиотека должна быть протестирована в рамках unit testing
4. Библиотека должна быть протестирована в рамках property-based тестирования
(как минимум 3 свойства, включая свойства моноида).
5. Структура должна быть полиморфной
6. Требуется использовать идиоматичный для технологии стиль программирования
7. Обратите внимание:
    - API должно быть реализовано для заданного интерфейса и оно не должно
    "протекать". На уровне тестов -- в первую очередь нужно протестировать
    именно API (dict, set, bag)
    - Должна быть эффективная реализация функции сравнения (не наивное
    приведение к спискам, их сортировка с последующим сравнением),
    реализованная на уровне API, а не внутреннего представления

## Ключевые элементы реализации

Структура представляющая узел дерева:

```haskell
data AVL a
  = Empty
  | Node
      { height :: Int
      , value  :: a
      , left   :: AVL a
      , right  :: AVL a
      }
  deriving (Show)
```

Было реализовано множество на основе AVL-дерева с основными операциями:

- `insert` - вставка элемента
- `delete` - удаление элемента
- `member` - проверка наличия элемента
- `union` - объединение множеств
- `intersection` - пересечение множеств
- `difference` - разность множеств
- `filter` - фильтрация элементов по предикату
- `map` - отображение элементов
- `foldl` и `foldr` - свертки
- `fromList` и `toList` - конвертация между списком и множеством
- `isSubsetOf` - проверка подмножества
- `equal` - проверка равенства множеств
- реализация моноида для множеств

### Выводы инструмента тестирования

```text
AvlSet Unit Tests
  Basic Operations
    empty set is empty [v]
    singleton set contains one element [v]
    insert adds element [v]
    insert duplicate doesn't increase size [v]
    delete removes element [v]
    delete non-existent element does nothing [v]
  Set Operations
    union combines sets [v]
    intersection finds common elements [v]
    difference removes elements [v]
    isSubsetOf checks subset relation [v]
  Higher-order Functions
    filter keeps matching elements [v]
    map transforms elements [v]
    foldl accumulates left to right [v]
    foldr accumulates right to left [v]
    foldl and foldr differ for non-commutative ops [v]
  Conversion
    fromList creates set from list [v]
    toList returns sorted unique elements [v]
  Equality
    equal sets are equal regardless of insertion order [v]
    different sets are not equal [v]
AvlSet Property-Based Tests
  Monoid Laws
    left identity: mempty <> x = x [v]
      +++ OK, passed 100 tests.
    right identity: x <> mempty = x [v]
      +++ OK, passed 100 tests.
    associativity: (x <> y) <> z = x <> (y <> z) [v]
      +++ OK, passed 100 tests.
  Set Properties
    member after insert is True [v]
      +++ OK, passed 100 tests.
    member after delete is False [v]
      +++ OK, passed 100 tests.
    size after insert increases by at most 1 [v]
      +++ OK, passed 100 tests.
    size after delete decreases by at most 1 [v]
      +++ OK, passed 100 tests.
    toList returns sorted unique elements [v]
      +++ OK, passed 100 tests.
    union is commutative [v]
      +++ OK, passed 100 tests.
    intersection is commutative [v]
      +++ OK, passed 100 tests.
    intersection is subset of both sets [v]
      +++ OK, passed 100 tests.
  Filter and Map Properties
    filter preserves subset relation [v]
      +++ OK, passed 100 tests.
    map preserves size or reduces it [v]
      +++ OK, passed 100 tests.
    filter with always True predicate returns same set [v]
      +++ OK, passed 100 tests.
    filter with always False predicate returns empty set [v]
      +++ OK, passed 100 tests.
  Fold Properties
    foldl and foldr with (+) are equal [v]
      +++ OK, passed 100 tests.
    foldl with list cons equals toList reversed [v]
      +++ OK, passed 100 tests.
    foldr with list cons equals toList [v]
      +++ OK, passed 100 tests.
  Equality Properties
    reflexivity: x == x [v]
      +++ OK, passed 100 tests.
    symmetry: if x == y then y == x [v]
      +++ OK, passed 100 tests.
    transitivity: if x == y and y == z then x == z [v]
      +++ OK, passed 100 tests.
    sets with same elements are equal [v]
      +++ OK, passed 100 tests.
AVL Tree Structural Properties
  Balance Properties
    AVL tree maintains balance after insertions [v]
      +++ OK, passed 100 tests.
    AVL tree maintains balance after deletions [v]
      +++ OK, passed 100 tests.
    height is logarithmic [v]
      +++ OK, passed 100 tests.

Finished in 0.1018 seconds
44 examples, 0 failures
```

## Выводы

Я познакомился с описанием стурктур данных и алгебраическими типами данных в
Haskell. Реализовал структуру данных AVL-дерево, а также основные операции над
ней. Были написаны юнит-тесты и property-based тесты для проверки корректности
реализации. В процессе работы я улучшил свои навыки программирования на Haskell
и научился использовать инструменты тестирования.
