(ns core
  (:refer-clojure :exclude [remove map]))

(defrecord RBNode [value color left right parent])
(defrecord RedBlackTree [root])

;; =============== SUPPORT FUNCTIONS ===============

(defn grandparent [node]
  (if (nil? (:parent node))
    nil
    (:parent (:parent node))))

(defn sibling [node]
  (if (nil? (:parent node))
    nil
    (if (= node (:left (:parent node)))
      (:right (:parent node))
      (:left (:parent node)))))

(defn uncle [node]
  (sibling (:parent node)))

(defn create-node [value]
  (->RBNode value :red nil nil nil))

(defn inorder-traversal [tree-or-node]
  (if (instance? RedBlackTree tree-or-node)
    (inorder-traversal (:root tree-or-node))
    (if tree-or-node
      (concat
       (inorder-traversal (:left tree-or-node))
       [(:value tree-or-node)]
       (inorder-traversal (:right tree-or-node)))
      [])))

(defn find-min [node]
  (if (nil? (:left node))
    node
    (recur (:left node))))

(defn insert-bst [root new-node]
  (if (nil? root)
    new-node
    (if (= (:value new-node) (:value root))
      root
      (if (< (compare (str (:value new-node)) (str (:value root))) 0)
        (assoc root
               :left (insert-bst (:left root) new-node)
               :parent root)
        (assoc root
               :right (insert-bst (:right root) new-node)
               :parent root)))))

;; ---------- balancing functions ----------

(defn rotate-left [node]
  (-> (:right node)
      (assoc :left (assoc node :right (:left (:right node))))
      (assoc :parent (:parent node))))

(defn rotate-right [node]
  (-> (:left node)
      (assoc :right (assoc node :left (:right (:left node))))
      (assoc :parent (:parent node))))

(defn balancing [node]
  (cond
    ;; 1. Узел – корень
    (nil? (:parent node))
    (assoc node :color :black)

    ;; 2. Родительский узел – черный
    (= (:color (:parent node)) :black)
    node

    ;; 3. Узел "дядя" – красный
    (and (uncle node) (= (:color (uncle node)) :red))
    (-> node
        (assoc-in [:parent :color] :black)
        (assoc-in [:uncle :color] :black)
        (assoc-in [:grandparent :color] :red)
        (balancing))

    ;; 4-5. Узел "дядя" черный или nil
    :else
    (let [is-left (= node (:left (:parent node)))
          is-parent-left (= (:parent node) (:left (grandparent node)))]
      (cond
        (and is-parent-left (not is-left))
        (balancing (rotate-left (:parent node)))

        (and (not is-parent-left) is-left)
        (balancing (rotate-right (:parent node)))

        :else
        (if is-parent-left
          (-> (grandparent node)
              rotate-right
              (assoc :color :red)
              (assoc-in [:left :color] :black))
          (-> (grandparent node)
              rotate-left
              (assoc :color :red)
              (assoc-in [:right :color] :black)))))))

;; ---------- remove functions ----------

;; Балансирова для случая
;;        Black
;;      /       \
;;  Black        Black
(defn balance-double-black [node]
  (let [sibling (sibling node)
        parent (:parent node)]
    (cond
      (= (:color sibling) :red)
      (let [new-parent (if (= node (:left parent))
                         (rotate-left parent)
                         (rotate-right parent))]
        (-> new-parent
            (assoc :color :black)
            (update :left #(if % (assoc % :color :red) %))
            (update :right #(if % (assoc % :color :red) %))
            balance-double-black))

      (and (= (:color sibling) :black)
           (= (:color (:left sibling)) :black)
           (= (:color (:right sibling)) :black))
      (-> node
          (assoc :color :black)
          (assoc-in [:parent :color]
                    (if (= (:color parent) :black) :double-black :black))
          (assoc-in [:parent :sibling :color] :red))

      (and (= (:color sibling) :black)
           (or (and (= node (:left parent))
                    (= (:color (:left sibling)) :red)
                    (= (:color (:right sibling)) :black))
               (and (= node (:right parent))
                    (= (:color (:right sibling)) :red)
                    (= (:color (:left sibling)) :black))))
      (let [new-sibling (if (= node (:left parent))
                          (rotate-right sibling)
                          (rotate-left sibling))]
        (-> new-sibling
            (assoc :color :black)
            (update :left #(if % (assoc % :color :red) %))
            (update :right #(if % (assoc % :color :red) %))
            balance-double-black))

      :else
      (let [new-parent (if (= node (:left parent))
                         (rotate-left parent)
                         (rotate-right parent))]
        (-> new-parent
            (assoc :color (:color parent))
            (update :left #(if % (assoc % :color :black) %))
            (update :right #(if % (assoc % :color :black) %))
            (assoc :color :black))))))

;; Функция для перекрашивания узлов
(defn balance-remove [node]
  (cond
    (= (:color node) :red)
    node

    (or (= (:color (:left node)) :red)
        (= (:color (:right node)) :red))
    (-> node
        (assoc :color :black)
        (update :left #(if % (assoc % :color :red) %))
        (update :right #(if % (assoc % :color :red) %)))

    :else
    (let [node (assoc node :color :double-black)]
      (balance-double-black node))))

;; Функция непосредственно для удаления узла
(defn remove-node [node value]
  (if (nil? node)
    nil
    (let [cmp (compare value (:value node))]
      (cond
        (< cmp 0)
        (let [new-left (remove-node (:left node) value)]
          (if (= new-left (:left node))
            node
            (balance-remove (assoc node :left new-left))))

        (> cmp 0)
        (let [new-right (remove-node (:right node) value)]
          (if (= new-right (:right node))
            node
            (balance-remove (assoc node :right new-right))))

        :else
        (cond
          (nil? (:left node))
          (let [right (:right node)]
            (if right (assoc right :parent (:parent node)) nil))

          (nil? (:right node))
          (let [left (:left node)]
            (if left (assoc left :parent (:parent node)) nil))

          :else
          (if (:parent node)
            (let [new-sibling (if (= node (:left (:parent node)))
                                (rotate-right (find-min (:right node)))
                                (rotate-left (find-min (:right node))))]
              (-> new-sibling
                  (assoc :color :black)
                  (update :left #(if % (assoc % :color :red) %))
                  (update :right #(if % (assoc % :color :red) %))
                  (balance-double-black)))
            (assoc node :color :black)))))))

;; ============ MAIN FUNCTIONS =============

;; • Добавление элементов;
(defn insert [tree value]
  (balancing (assoc tree :root (insert-bst (:root tree) (create-node value)))))

;; • Нейтральный элемент (пустое дерево)
(def empty-tree (->RedBlackTree nil))

;; • Удаление элементов;
(defn remove [tree value]
  (if-let [root (:root tree)]
    (if (nil? (remove-node root value))
      (empty-tree)
      (->RedBlackTree (assoc (remove-node root value) :color :black)))
    tree))

;; • Отображение (map);
(defn map [f tree]
  (letfn [(map-node [node]
            (if (nil? node)
              nil
              (assoc node
                     :value (f (:value node))
                     :left (map-node (:left node))
                     :right (map-node (:right node)))))]
    (->RedBlackTree (map-node (:root tree)))))

;; • Левая свертка;
(defn fold-left [f init tree]
  (letfn [(fold-node [acc node]
            (if (nil? node)
              acc
              (-> acc
                  (fold-node (:left node))
                  (f (:value node))
                  (fold-node (:right node)))))]
    (fold-node init (:root tree))))

;; • Правая свертка;
(defn fold-right [f init tree]
  (letfn [(fold-node [node]
            (if (nil? node)
              init
              (f (:value node)
                 (fold-node (:right node)))))]
    (fold-node (:root tree))))

;; • Свойства моноида;
;; Ассоциативная бинарная операция (+)
(defn merge-trees [tree1 tree2]
  (letfn [(insert-all [target-tree source-tree]
            (if (nil? (:root source-tree))
              target-tree
              (reduce insert target-tree (inorder-traversal (:root source-tree)))))]
    (insert-all tree1 tree2)))

;; • Фильтрация;
(defn filter-tree [pred tree]
  (reduce (fn [acc val] (insert acc val)) empty-tree (filter pred (inorder-traversal tree))))
