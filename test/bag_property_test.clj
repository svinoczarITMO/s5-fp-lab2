(ns bag-property-test
  (:require [clojure.test :refer [deftest is run-tests]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [bag :refer :all]))


(def gen-bag
  (gen/fmap
    (fn [items]
      (reduce #(add %1 %2) (empty-bag) items))
    (gen/vector gen/small-integer)))

;; Monoid
(defspec merge-empty-bag-test 100
  (prop/for-all [bag gen-bag]
    (let [empty (empty-bag)]
      (and (= (get-contents (merge-bags empty bag))
              (get-contents bag))
           (= (get-contents (merge-bags bag empty))
              (get-contents bag))))))

;; Idempotent
(defspec idempotent-add-test 100
  (prop/for-all [element gen/small-integer
                 bag gen-bag]
    (let [new-bag (add (add bag element) element)
          count-before (count (filter #(= % element) (get-contents bag)))
          count-after (count (filter #(= % element) (get-contents new-bag)))]
      (= count-after (+ count-before 2)))))


;; Filter
(defspec filter-test 100
  (prop/for-all [bag gen-bag]
    (let [filtered (filter-bag bag (fn [x] (and (integer? x) (even? x))))
          contents (get-contents filtered)]
      (and (every? #(and (integer? %) (even? %)) contents)
           (= (count contents)
              (count (filter #(and (integer? %) (even? %)) (get-contents bag))))))))

;; Fold
(defspec fold-consistency-test 100
  (prop/for-all [bag gen-bag]
    (= (fold-left bag + 0)
       (reduce + 0 (get-contents bag)))))

(run-tests)