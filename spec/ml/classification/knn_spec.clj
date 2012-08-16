(ns ml.classification.knn-spec
  (:require [ml.classification.knn :as knn])
  (:use ml.helpers
        incanter.core
        clojure.test
        [clojure.math.numeric-tower :only (round)]))

(def data 
  (let [raw-data (read-data "data/knn.csv")]
    {:xs (to-matrix (sel raw-data :cols [0 1]))
     :ys (sel raw-data :cols 2)}))

(let [{:keys [xs ys]} data]
  (deftest predict-nearest-neighbor
    (testing "returns 1 if closer to points labeled 1"
      (let [classification (knn/classify xs ys [0.2 0.5] 3)]
        (is (= classification 1.0))))
    (testing "returns 0 if closer to points labeled 0"
      (let [classification (knn/classify xs ys [1.2 1.5] 3)]
        (is (= classification 0.0))))))
