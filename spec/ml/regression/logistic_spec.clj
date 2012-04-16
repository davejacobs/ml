(ns ml.regression.logistic-spec
  (:require [ml.regression.logistic :as logistic])
  (:use ml.helpers 
        incanter.core
        clojure.test
        [clojure.math.numeric-tower :only (round)]))

(def data 
  (let [raw-data (read-data "data/ex2.1")
        m (first (dim raw-data))
        xs-first (vec (take m (repeat 1)))
        xs-rest (to-matrix (sel raw-data :cols [0 1]))
        xs (bind-columns xs-first xs-rest)
        ys (sel raw-data :cols 2)]
    {:xs xs 
     :ys ys
     :m m
     :iterations 100
     :alpha 0.01}))

(def expected-xs
  (matrix [[1.0 34.6237 78.0247]
           [1.0 30.2867 43.8950]
           [1.0 35.8474 72.9022]
           [1.0 60.1826 86.3086]]))

(def expected-ys [0 0 0 1])

(deftest data-import
  (testing "data is read properly"
    (let [{:keys [xs ys m]} data]
      (is (= m 100))
      (is (matrices-equal? (take 4 xs) expected-xs))
      (is (matrices-equal? (take 4 ys) expected-ys)))))
