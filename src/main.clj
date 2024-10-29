(ns main
    (:require [core :refer :all]))

(defn check-monoid-properties []
  (let [tree1 (-> empty-tree (insert 1) (insert 2) (insert 3))
        tree2 (-> empty-tree (insert 4) (insert 5))
        tree3 (-> empty-tree (insert 6) (insert 7) (insert 8))]
        
    (println "a. Нейтральный элемент:")
    (println "tree1 + empty-tree == tree1:" (= (inorder-traversal (:root (merge-trees tree1 empty-tree)))
                (inorder-traversal (:root tree1))))
    (println "empty-tree + tree1 == tree1:" (= (inorder-traversal (:root (merge-trees empty-tree tree1)))
                (inorder-traversal (:root tree1))))
    
    (println "\nb. Ассоциативность:")
    (println "(tree1 + tree2) + tree3 == tree1 + (tree2 + tree3):" 
        (let [left-assoc (merge-trees (merge-trees tree1 tree2) tree3)
              right-assoc (merge-trees tree1 (merge-trees tree2 tree3))]
                (= (inorder-traversal (:root left-assoc)) (inorder-traversal (:root right-assoc)))))
    
))

(defn -main []
  ;; Создание и добавление
    (let [tree (-> (->RedBlackTree nil)
                 (insert 10)
                 (insert 20)
                 (insert 30)
                 (insert 15)
                 (insert 25))]
    
    (println "1. Исходное дерево:")
    (println (inorder-traversal (:root tree)))
    
    ;; Удаление
    (let [tree-after-delete (delete tree 20)]
      (println "\n2. Дерево после удаления элемента 20:")
      (println (inorder-traversal (:root tree-after-delete)))) ; Выводим результат после удаления
    
    ;; Отображение 
    (let [doubled-tree (map #(* 2 %) tree)]
      (println "\n3. Дерево после умножения всех элементов на 2:")
      (println (inorder-traversal (:root doubled-tree))))
    
    ;; Левая свертка (+)
    (let [sum (fold-left + 0 tree)]
      (println "\n4. Сумма всех элементов (левая свертка):")
      (println sum))
    
    ;; Правая свертка (*)
    (let [product (fold-right * 1 tree)]
      (println "\n5. Произведение всех элементов (правая свертка):")
      (println product))
    
    ;; Вставка нового элемента
    (let [tree-with-new-element (insert tree 23)]
      (println "\n6. Дерево после вставки элемента 23:")
      (println (inorder-traversal (:root tree-with-new-element))))
      
      (println "\nПРОВЕРКА НА 'МОНОИДНОСТЬ':")
      (check-monoid-properties)))

(-main)