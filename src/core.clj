(ns core
  (:refer-clojure :exclude [map]))

(defrecord RBNode [value color left right parent])

(defrecord RedBlackTree [root])

;; =============== SUPPORT FUNCTIONS =============== 

(defn grandparent [node]
  (if (nil? (:parent node)) nil
      (:parent (:parent node))))

(defn sibling [node]
  (if (nil? (:parent node)) nil
      (if (= node (:left (:parent node)))
        (:right (:parent node))
        (:left (:parent node)))))

(defn uncle [node]
  (sibling (:parent node)))

(defn create-node [value]
  (->RBNode value :red nil nil nil))

(defn inorder-traversal [node]
  (if node
    (concat 
      (inorder-traversal (:left node))
      [(:value node)]
      (inorder-traversal (:right node)))
    []))

(defn find-min [node]
  (if (nil? (:left node))
    node
    (recur (:left node))))

(defn insert-bst [root new-node]
  (if (nil? root)
    new-node
    (let [parent-value (:value root)
          new-value (:value new-node)]
      (if (< new-value parent-value)
        (let [new-left (insert-bst (:left root) new-node)]
          (assoc root 
                 :left new-left
                 :parent root))
        (let [new-right (insert-bst (:right root) new-node)]
          (assoc root 
                 :right new-right
                 :parent root))))))


;; ---------- balancing functions ---------- 
(defn rotate-left [node]
  (let [right-child (:right node)
        right-left (:left right-child)]
    (-> right-child
        (assoc :left (assoc node :right right-left))
        (assoc :parent (:parent node)))))

(defn rotate-right [node]
  (let [left-child (:left node)
        left-right (:right left-child)]
    (-> left-child
        (assoc :right (assoc node :left left-right))
        (assoc :parent (:parent node)))))

(defn balancing [node]
  (let [parent (:parent node)
        uncle-node (uncle node)
        grandparent-node (grandparent node)]
    (cond
      ;; 1. Узел – корень
      (nil? parent) 
      (assoc node :color :black)
      
      ;; 2. Родительский узел – черный
      (= (:color parent) :black) 
      node
      
      ;; 3. Узел "дядя" – красный
      (and uncle-node (= (:color uncle-node) :red))
      (-> node
          (assoc-in [:parent :color] :black)
          (assoc-in [:uncle :color] :black)
          (assoc-in [:grandparent :color] :red)
          (balancing grandparent-node))
      
      ;; 4-5. Узел "дядя" черный или nil
      :else
      (let [is-left (= node (:left parent))
            is-parent-left (= parent (:left grandparent-node))]
        (cond
          (and is-parent-left (not is-left))
          (balancing (rotate-left parent))
          
          (and (not is-parent-left) is-left)
          (balancing (rotate-right parent))
          
          :else
          (if is-parent-left
            (-> grandparent-node
                rotate-right
                (assoc :color :red)
                (assoc-in [:left :color] :black))
            (-> grandparent-node
                rotate-left
                (assoc :color :red)
                (assoc-in [:right :color] :black))))))))


;; ---------- delete functions ---------- 

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
(defn balance-delete [node]
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
(defn delete-node [node value]
  (cond
    (nil? node) 
    nil

    (< value (:value node))
    (let [new-left (delete-node (:left node) value)]
      (if (= (:color node) :black)
        (balance-delete (assoc node :left new-left))
        (assoc node :left new-left)))

    (> value (:value node))
    (let [new-right (delete-node (:right node) value)]
      (if (= (:color node) :black)
        (balance-delete (assoc node :right new-right))
        (assoc node :right new-right)))

    :else
    (cond
      (and (nil? (:left node)) (nil? (:right node)))
      nil
      
      (nil? (:left node))
      (let [child (:right node)]
        (if (= (:color node) :black)
          (balance-delete (assoc child :color :black))
          child))
      
      (nil? (:right node))
      (let [child (:left node)]
        (if (= (:color node) :black)
          (balance-delete (assoc child :color :black))
          child))
      
      :else
      (let [successor (find-min (:right node))
            new-node (assoc node :value (:value successor))
            new-right (delete-node (:right new-node) (:value successor))]
        (if (= (:color new-node) :black)
          (balance-delete (assoc new-node :right new-right))
          (assoc new-node :right new-right))))))


;; ============ MAIN FUNCTIONS =============

;; • Добавление элементов;
(defn insert [tree value]
  (let [new-node (create-node value)]
    (if (nil? (:root tree))
      (->RedBlackTree (assoc new-node :color :black))
      (let [new-root (insert-bst (:root tree) new-node)
            fixed-root (balancing new-root)]
        (->RedBlackTree fixed-root)))))

;; • Удаление элементов;
(defn delete [tree value]
    (let [new-root (delete-node (:root tree) value)] 
      (->RedBlackTree 
       (if new-root 
         (assoc new-root :color :black) 
         nil))))

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
                 (fold-node (:left node))
                 (fold-node (:right node)))))]
    (fold-node (:root tree))))

;; • Свойства моноида;
;; Ассоциативная бинарная операция (+) 
(defn merge-trees [tree1 tree2]
  (letfn [(insert-all [target-tree source-tree]
            (if (nil? (:root source-tree))
              target-tree
              (let [values (inorder-traversal (:root source-tree))]
                (reduce insert target-tree values))))]
    (insert-all tree1 tree2)))

;; Нейтральный элемент (пустое дерево)
(def empty-tree (->RedBlackTree nil))