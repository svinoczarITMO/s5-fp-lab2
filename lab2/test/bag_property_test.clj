(ns bag-property-test
  (:require [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [bag :refer [add empty-bag filter-bag fold-left get-contents merge-bags]]))

(def gen-bag
  (gen/fmap
   (fn [items]
     (reduce #(add %1 %2) (empty-bag) items))
   (gen/vector gen/small-integer)))

;; ===============================TESTS=======================================

;; Monoid
(defn merge-empty-bag-test []
  (defspec merge-empty-bag-test 100
    (prop/for-all [bag gen-bag]
                  (let [empty (empty-bag)]
                    (and (= (get-contents (merge-bags empty bag))
                            (get-contents bag))
                         (= (get-contents (merge-bags bag empty))
                            (get-contents bag)))))))

;; Polymorphic
(defn polymorphic-test []
  (defspec polymorphic-test 100
    (prop/for-all [element (gen/one-of [gen/int gen/string gen/boolean])
                   bag gen-bag]
                  (let [new-bag (add bag element)
                        contents (get-contents new-bag)]
                    (some #(= element %) contents)))))

;; Associative
(defn merge-associative []
  (defspec merge-associative 100
    (prop/for-all [bag1 gen-bag
                   bag2 gen-bag
                   bag3 gen-bag]
                  (= (get-contents (merge-bags (merge-bags bag1 bag2) bag3))
                     (get-contents (merge-bags bag1 (merge-bags bag2 bag3)))))))

;; Filter
(defn filter-test []
  (defspec filter-test 100
    (prop/for-all [bag gen-bag]
                  (let [filtered (filter-bag bag (fn [x] (and (integer? x) (even? x))))
                        contents (get-contents filtered)]
                    (and (every? #(and (integer? %) (even? %)) contents)
                         (= (count contents)
                            (count (filter #(and (integer? %) (even? %)) (get-contents bag)))))))))

;; Fold
(defn fold-consistency-test []
  (defspec fold-consistency-test 100
    (prop/for-all [bag gen-bag]
                  (= (fold-left bag + 0)
                     (reduce + 0 (get-contents bag))))))