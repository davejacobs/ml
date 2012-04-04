(ns ml.regression.logistic-spec
  (:require [ml.regression.logistic :as logistic])
  (:use incanter.core
        ml.helpers
        clojure.test
        [clojure.math.numeric-tower :only (round)]))

(def data 
  (let [raw-data (read-data "data/ex2.1")
        y (sel raw-data :cols 2)
        m (count y)
        x (let [x-first (vec (take m (repeat 1)))
                x-rest (to-matrix (sel raw-data :cols [0 1]))]
            (bind-columns x-first x-rest))
        theta (matrix [[0] [0] [0]])
        iterations 1500
        alpha 0.01]
    {:x x 
     :y y 
     :theta theta 
     :iterations iterations 
     :alpha alpha}))

(def expected-x
  (matrix [[1.0000 34.6237 78.0247]
           [1.0000 30.2867 43.8950]
           [1.0000 35.8474 72.9022]]))

(def expected-y [0 0 0])

(deftest data-import
  (testing "data is read properly"
    (let [{:keys [x y theta iterations alpha]} data]
      (is (= (count x) 100))
      (is (matrices-equal? (take 3 y) expected-y))
      (is (matrices-equal? (take 3 x) expected-x)))))
(deftest calculate-initial-cost
  (testing "calculates the cost of thetas relating x and y"
    (let [{:keys [x y]} data
          initial-cost (logistic/cost x y)]
      (is (= initial-cost 0.69315)))))
