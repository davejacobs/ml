(ns ml.exercise1.core-spec
  (:use ml.exercise1.core 
        clojure.test
        incanter.core
        [clojure.math.numeric-tower :only (round)]))

(def X
  (matrix [[1 1 3]
           [1 2 2]
           [1 3 1]]))

(def y [10 20 10])

(def theta (matrix [[0] [0] [0]]))

(def expected-normalized-X
  (matrix [[1.0 -1.0  1.0]
           [1.0  0.0  0.0]
           [1.0  1.0 -1.0]]))

(def expected-next-theta
  (matrix [[0.1333]
           [0.2667]
           [0.2667]]))

(def expected-final-theta
  (matrix [[1.4814]
           [2.9627]
           [2.9627]]))

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
    (let [exact-cost (cost X y theta)
          rounded-cost (round exact-cost)] 
      (is (= rounded-cost 100)))))

(deftest calculate-next-theta
  (testing "calculates the next set of thetas based on gradient descent"
    (is (= (next-theta X y theta 0.01) expected-next-theta))))

(deftest minimize-theta
  (testing "minimizes theta over the iterations given"
    (let [res (gradient-descent X y theta 0.01 100)
          {final-theta :theta history :history} res]
      (is (= final-theta expected-final-theta)))))
