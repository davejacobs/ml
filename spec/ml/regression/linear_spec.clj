(ns ml.regression.linear-spec
  (:require [ml.regression.linear :as linear])
  (:use ml.helpers
        incanter.core
        clojure.test
        [clojure.math.numeric-tower :only (round)]))

(def data 
  (let [raw-data (read-data "data/ex1.2")
        m (first (dim raw-data))
        x-first (vec (take m (repeat 1)))
        x-rest (to-matrix (sel raw-data :cols [0 1]))
        x (bind-columns x-first x-rest)]
    {:x x 
     :y (sel raw-data :cols 2) 
     :theta (matrix [[0] [0] [0]]) 
     :iterations 100
     :alpha 0.01}))

(def expected-x
  (matrix [[1.0 2104.0 3.0]
           [1.0 1600.0 3.0]
           [1.0 2400.0 3.0]
           [1.0 1416.0 2.0]]))

(def expected-y [399900 329900 369000 232000])

(def expected-normalized-x
  (matrix [[1.0000 0.1300 -0.2237]
           [1.0000 -0.5042 -0.2237]]))

(def expected-first-theta
  (matrix [[3404.1266]
           [1046.3293]
           [541.2369]])) 

(def expected-last-theta
  (matrix [[215810.6168] 
           [61384.0308]
           [20273.5507]]))

(deftest data-import
  (testing "data is read properly"
    (let [{:keys [x y theta iterations alpha]} data]
      (is (matrices-equal? (take 4 x) expected-x))
      (is (matrices-equal? (take 4 y) expected-y)))))

(deftest normalize-one-value
  (testing "properly scales one value via mean and standard deviation"
    (is (= (linear/normalize 1 2 1) -1.0)))) 

(deftest normalize-one-vector
  (testing "passes through an empty vector"
    (is (= (linear/normalize-vector []) [])))
  (testing "properly scales all values in a column"
    (let [normalized-vector (linear/normalize-vector [1 2 3])]
      (is (= normalized-vector [-1.0 0.0 1.0])))))

(deftest normalize-one-matrix
  (testing "properly scales all features"
    (let [normalized-x (linear/normalize-matrix (data :x))]
      (is (matrices-equal? (take 2 normalized-x)
                           expected-normalized-x)))))

(deftest calculate-cost-of-prediction-theta
  (testing "calculates the cost of predictions theta relating x and y"
    (let [{:keys [x y theta]} data
          normalized-x (linear/normalize-matrix x)
          exact-cost (linear/cost normalized-x y theta)
          rounded-cost (round exact-cost)] 
      (is (close-to? rounded-cost 65591548106 -1)))))

(deftest calculate-next-theta
  (testing "calculates the next set of thetas based on gradient descent"
    (let [{:keys [x y theta]} data
          normalized-x (linear/normalize-matrix x)]
      (is (matrices-equal? (linear/next-theta normalized-x y theta 0.01) expected-first-theta)))))

(deftest minimize-theta
  (testing "minimizes theta over the iterations given"
    (let [{:keys [x y theta alpha iterations]} data
          normalized-x (linear/normalize-matrix x)
          res (linear/gradient-descent normalized-x y theta alpha iterations)
          {last-theta :theta history :history} res]
      (is (matrices-equal? last-theta expected-last-theta)))))
