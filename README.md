**Студент:** Бабушкин Александр Михайлович  
**ИСУ:** 367845
**Группа:** P3321  
**Университет:** НИУ ИТМО  
**Факультет:** Программной инженерии и компьютерной техники
**Курс:** 3-й курс  
  
---
  
# Отчет по лабораторной работе: Реализация неизменяемого мешка (Separate Chaining Hashmap)

Цель данной лабораторной работы заключается в ознакомлении с построением пользовательских типов данных, полиморфизмом и средствами тестирования (unit testing и property-based testing). В рамках работы была реализована структура данных — красно-черный мешок (rb-bag).

## Требования к разработанному ПО

1. Реализовать функции:
   - добавление и удаление элементов;
   - фильтрация;
   - отображение (map);
   - свертки (левая и правая);
   - структура должна быть моноидом.

2. Структуры данных должны быть неизменяемыми.
3. Библиотека должна быть протестирована в рамках unit testing.
4. Библиотека должна быть протестирована в рамках property-based testing (как минимум 3 свойства, включая свойства моноида).
5. Структура должна быть полиморфной.
6. Использовать идиоматичный для технологии стиль программирования.

## Ключевые элементы реализации

### Определение структуры данных

Создание пустого мешка:

```clojure
(defn empty-bag []
  (->Bag tree/empty-tree))
```

### Добавление элемента

Функция `add` добавляет элемент в мешок. 
Для этого используется функция `create-node`, которая создает новый узел в красно-черном дереве:

```clojure
(defn insert [tree value]
  (let [new-node (create-node value)]
    (if (nil? (:root tree))
      (do
        (->RedBlackTree (assoc new-node :color :black)))
      (let [new-root (insert-bst (:root tree) new-node)
            fixed-root (balancing new-root)]
        (->RedBlackTree fixed-root)))))
```

### Удаление элемента

Функция `remove` удаляет один элемент из мешка. Она использует функцию `remove-node` для удаления узла и перебалансировки дерева:

```clojure
(defn remove [tree value]
  (if-let [new-root (remove-node (:root tree) value)]
    (->RedBlackTree (assoc new-root :color :black))
    (->RedBlackTree nil)))
```

### Фильтрация

Функция `filter-tree` фильтрует элементы мешка по заданному предикату. Оставляет в дереве только подходящие узлы, балансирует его и возвращает новое дерево:

```clojure
(defn filter-tree [pred tree]
  (letfn [(filter-node [node]
            (if (nil? node)
              nil
              (if (pred (:value node))
                (let [new-node (->RBNode (:value node) (:color node)
                                        (filter-node (:left node))
                                        (filter-node (:right node))
                                        (:parent node))]
                  (balancing new-node))
                (let [left-result (filter-node (:left node))
                      right-result (filter-node (:right node))]
                  (cond
                    (and left-result right-result) 
                    (merge-trees (->RedBlackTree left-result) 
                                (->RedBlackTree right-result))
                    
                    left-result left-result
                    right-result right-result
                    :else nil)))))]
    (if-let [filtered-root (filter-node (:root tree))]
      (->RedBlackTree (assoc filtered-root :color :black))
      empty-tree)))
```

### Отображение (map)

Функция `map` применяет функцию к элементам мешка.
Применяет функцию ко всем узлам и возвращает новое отбалансированное дерево:

```clojure
(defn map [f tree]
  (letfn [(map-node [node]
            (if (nil? node)
              nil
              (assoc node
                     :value (f (:value node))
                     :left (map-node (:left node))
                     :right (map-node (:right node)))))]
    (->RedBlackTree (map-node (:root tree)))))
```

### Свертки

Левая и правая свертки реализованы в функциях `fold-left` и `fold-right` соответственно:

```clojure
(defn fold-left [f init tree]
  (letfn [(fold-node [acc node]
            (if (nil? node)
              acc
              (-> acc
                  (fold-node (:left node))
                  (f (:value node))
                  (fold-node (:right node)))))]
    (fold-node init (:root tree))))
```

```clojure
(defn fold-right [f init tree]
  (letfn [(fold-node [node]
            (if (nil? node)
              init
              (f (:value node)
                 (fold-node (:right node)))))]
    (fold-node (:root tree))))
```

### Неизменяемость

Коллекция неизменяемая, поскольку все функции возвращают новые экземпляры деревьев без изменения исходного.  
Нейтральный элемент:

```clojure
(def empty-tree (->RedBlackTree nil))
```

## Тестирование

### Unit Testing

Для тестирования основных функций были реализованы следующие [тесты](test/bag-test.clj).

### Property-Based Testing

Для тестирования свойств были реализованы следующие [тесты](test/bag-property-test.clj).

## Выводы

В процессе выполнения лабораторной работы я изучил создание неизменяемых структур данных на языке Clojure. Реализация мешка с использованием красно-черных деревьев позволила углубить понимание работы с рекурсивными алгоритмами и полиморфизмом. Применение спецификаций и свойств для тестирования обеспечило надежность и стабильность библиотеки, что позволило уверенно добавлять новые функции.