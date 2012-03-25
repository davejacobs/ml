(ns ml.exercise1.core-spec
  (:use ml.exercise1.core 
        [clojure.math.numeric-tower :only (round)]
        clojure.test
        incanter.core))

(def X
  (matrix [[1 1 3]
           [1 2 2]
           [1 3 1]]))

(def y [10 20 10])

(def expected-normalized-X
  (matrix [[1.0 -1.0  1.0]
           [1.0  0.0  0.0]
           [1.0  1.0 -1.0]]))

(deftest normalize-one-value
  (testing "properly scales one value via mean and standard deviation"
    (is (= (normalize 1 2 1) -1.0)))) 

(deftest normalize-one-column
  (testing "passes through an empty column"
    (is (= (normalize-vector []) [])))
  (testing "properly scales all values in a column"
    (let [[a b c] (normalize-vector [1 2 3])]
      (is (= a -1.0))
      (is (= b 0.0))
      (is (= c 1.0)))))

(deftest normalize-one-matrix
  (testing "properly scales all features"
    (is (= (normalize-matrix X) expected-normalized-X))))

(deftest calculate-cost-of-prediction-theta
  (testing "calculates the cost of predictions theta relating X and y"
    (let [exact-cost (cost X y [[0] [0] [0]])
          rounded-cost (round exact-cost)] 
      (is (= rounded-cost 100)))))
