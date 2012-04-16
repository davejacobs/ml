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

(def expected-first-cost-prime
  (matrix [[ -0.1000]
           [-12.0092]
           [-11.2628]]))

(def expected-final-thetas
  (matrix [[0]
           [0]
           [0]]))

(deftest data-import
  (testing "data is read properly"
    (let [{:keys [xs ys m]} data]
      (is (= m 100))
      (is (matrices-equal? (take 4 xs) expected-xs))
      (is (matrices-equal? (take 4 ys) expected-ys)))))

(deftest calculate-first-cost-prime
  (testing "calculates the first cost gradient"
    (let [{:keys [xs ys]} data
          thetas (matrix [[0] [0] [0]])
          first-cost-prime (logistic/cost-prime xs ys thetas)]
      (is (matrices-equal? first-cost-prime expected-first-cost-prime)))))

(deftest calculate-first-thetas
  (testing "calculates the first thetas"
    (let [{:keys [xs ys]} data
          thetas (matrix [[0] [0] [0]])
          first-thetas (logistic/next-thetas xs ys thetas)]
      (is (matrices-equal? first-thetas (minus expected-first-cost-prime))))))

(deftest calculate-initial-cost
  (testing "calculates the cost of thetas relating xs and ys"
    (let [{:keys [xs ys]} data
          thetas (matrix [[0] [0] [0]])
          first-cost (logistic/cost xs ys thetas)]
      (is (close-to? first-cost 0.693147)))))
