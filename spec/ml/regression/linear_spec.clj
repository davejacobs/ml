(ns ml.regression.linear-spec
  (:require [ml.regression.linear :as linear])
  (:use ml.helpers
        incanter.core
        clojure.test
        [clojure.math.numeric-tower :only (round)]))

(def data 
  (let [raw-data (read-data "data/ex1.2")
        m (first (dim raw-data))
        xs (with-bias-unit (sel raw-data :cols [0 1]))
        ys (sel raw-data :cols 2) 
        normalized-xs (linear/normalize-matrix xs)]
    {:xs xs
     :normalized-xs normalized-xs
     :ys ys
     :m m
     :thetas (matrix [[0] [0] [0]]) 
     :iterations 100
     :alpha 0.01}))

(def expected-xs
  (matrix [[1.0 2104.0 3.0]
           [1.0 1600.0 3.0]
           [1.0 2400.0 3.0]
           [1.0 1416.0 2.0]]))

(def expected-ys [399900 329900 369000 232000])

(def expected-normalized-xs
  (matrix [[1.0000 0.1300 -0.2237]
           [1.0000 -0.5042 -0.2237]
           [1.0000 0.5025 -0.2237]
           [1.0000 -0.7357 -1.5378]]))

(def expected-first-thetas
  (matrix [[3404.1266]
           [1046.3293]
           [541.2369]])) 

(def expected-last-thetas
  (matrix [[215810.6168] 
           [61384.0308]
           [20273.5507]]))

(deftest data-import
  (testing "data is read properly"
    (let [{:keys [xs ys m]} data]
      (is (= m 47))
      (is (matrices-equal? (take 4 xs) expected-xs))
      (is (matrices-equal? (take 4 ys) expected-ys)))))

(deftest normalize-one-value
  (testing "passes through nil"
    (is (= (linear/normalize nil nil nil) nil)))
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
    (let [normalized-xs (linear/normalize-matrix (data :xs))]
      (is (matrices-equal? (take 4 normalized-xs)
                           expected-normalized-xs)))))

(deftest calculate-cost-of-prediction-thetas
  (testing "calculates the cost of predictions thetas relating xs and ys"
    (let [{:keys [normalized-xs ys thetas]} data
          exact-cost (linear/cost normalized-xs ys thetas)
          rounded-cost (round exact-cost)] 
      (is (close-to? rounded-cost 65591548106 -1)))))

(deftest calculate-next-thetas
  (testing "calculates the next set of thetas based on gradient descent"
    (let [{:keys [normalized-xs ys thetas alpha]} data]
      (is (matrices-equal? (linear/next-thetas normalized-xs ys thetas alpha) 
                           expected-first-thetas)))))

(deftest minimize-thetas
  (testing "minimizes thetas over the iterations given"
    (let [{:keys [normalized-xs ys thetas alpha iterations]} data
          res (linear/gradient-descent normalized-xs ys alpha iterations)
          {last-thetas :thetas history :history} res]
      (is (matrices-equal? last-thetas expected-last-thetas)))))
