(ns bag
  (:require [core :as tree]
            [clojure.core :as core])
  (:import [core RedBlackTree]))

(defprotocol BagProtocol
  (add [this value] "Добавить элемент в мешок.")
  (remove-bag [this value] "Удалить один экземпляр элемента из мешка.")
  (merge-bags [this other] "Объединить два мешка.")
  (filter-bag [this pred] "Фильтровать элементы в мешке по предикату.")
  (fold-left [this f init] "Левая свертка.")
  (fold-right [this f init] "Правая свертка.")
  (get-contents [this] "Отображение мешка в виде вектора."))


(defrecord Bag [tree]
  BagProtocol
  (add [this value]
    (->Bag (tree/insert (:tree this) value)))
  
  (remove-bag [this value]
    (->Bag (tree/remove (:tree this) value)))

  (merge-bags [this other]
    (->Bag (tree/merge-trees (:tree this) (:tree other))))
  
  (filter-bag [this pred]
    (->Bag (tree/filter-tree pred (:tree this))))
  
  (fold-left [this f init]
    (tree/fold-left f init (:tree this)))
  
  (fold-right [this f init]
    (tree/fold-right f init (:tree this)))
    
  (get-contents [this]
    (if (:root (:tree this))
      (tree/inorder-traversal (:tree this))
      [])))

(defn empty-bag []
  (->Bag tree/empty-tree))