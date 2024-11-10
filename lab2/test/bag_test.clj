(ns bag-test
  (:require [clojure.test :refer [deftest is run-tests]]
            [bag :refer [add empty-bag filter-bag fold-left fold-right get-contents merge-bags remove-bag]]
            [core :as tree]
            [clojure.string :as str]))

(deftest test-add-integers
  (let [bag (empty-bag)
        updated-bag (add bag 1)]
    (is (= (get-contents updated-bag) [1]))))

(deftest test-add-strings
  (let [bag (empty-bag)
        updated-bag (add bag "func")]
    (is (= (get-contents updated-bag) ["func"]))))

(deftest test-remove-integers
  (let [bag (-> (empty-bag)
                (add 1)
                (add 2))
        updated-bag (remove-bag bag 1)]
    (is (= (get-contents updated-bag) [2]))))

(deftest test-remove-strings
  (let [bag (-> (empty-bag)
                (add "func")
                (add "prog"))
        updated-bag (remove-bag bag "func")]
    (is (= (get-contents updated-bag) ["prog"]))))

(deftest test-merge-integers
  (let [bag1 (-> (empty-bag)
                 (add 1))
        bag2 (-> (empty-bag)
                 (add 2))
        merged-bag (merge-bags bag1 bag2)]
    (is (= (tree/inorder-traversal (-> merged-bag :tree :root)) [1 2]))))

(deftest test-merge-strings
  (let [bag1 (-> (empty-bag)
                 (add "func"))
        bag2 (-> (empty-bag)
                 (add "prog"))
        merged-bag (merge-bags bag1 bag2)]
    (is (= (tree/inorder-traversal (-> merged-bag :tree :root)) ["func" "prog"]))))

(deftest test-filter-integers
  (let [bag (-> (empty-bag)
                (add 1)
                (add 2))
        filtered-bag (filter-bag bag #(= % 1))]
    (is (= (tree/inorder-traversal (-> filtered-bag :tree :root)) [1]))))

(deftest test-filter-strings
  (let [bag (-> (empty-bag)
                (add "func")
                (add "prog"))
        filtered-bag (filter-bag bag #(= % "func"))]
    (is (= (tree/inorder-traversal (-> filtered-bag :tree :root)) ["func"]))))

(deftest test-fold-left-integers
  (let [bag (-> (empty-bag)
                (add 1)
                (add 2)
                (add 3))]
    (is (= (fold-left bag + 0) 6))))

(deftest test-fold-left-strings
  (let [bag (-> (empty-bag)
                (add "func")
                (add "prog"))]
    (is (= (fold-left bag #(if (empty? %1) %2 (str %1 " " %2)) "") "func prog"))))

(deftest test-fold-right-integers
  (let [bag (-> (empty-bag)
                (add 1)
                (add 2)
                (add 3))]
    (is (= (fold-right bag (fn [x acc] (- x acc)) 0) 2))))

(deftest test-fold-right-strings
  (let [bag (-> (empty-bag)
                (add "func")
                (add "prog"))]
    (is (= (fold-right bag #(if (empty? %2) %1 (str %2 " " %1)) "") "prog func"))))

(deftest test-map-integers
  (let [bag (-> (empty-bag)
                (add 51)
                (add 52)
                (add 53))
        mapped-bag (tree/map inc (:tree bag))]
    (is (= (tree/inorder-traversal mapped-bag) [52 53 54]))))

(deftest test-map-strings
  (let [bag (-> (empty-bag)
                (add "f")
                (add "p"))
        mapped-bag (tree/map str/upper-case (:tree bag))]
    (is (= (tree/inorder-traversal mapped-bag) ["F" "P"]))))

(run-tests)