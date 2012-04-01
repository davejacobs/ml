(ns ml.regression.linear-spec
  (:use ml.regression.linear
        ml.helpers
        incanter.core
        clojure.test
        [clojure.math.numeric-tower :only (round)]))

(def x
  (matrix [[1 1 3]
           [1 2 2]
           [1 3 1]]))

(def y [10 20 10])

(def theta 
  (matrix [[0]
           [0]
           [0]]))

(def expected-normalized-x
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
    (is (= (normalize-matrix x) expected-normalized-x))))

(deftest calculate-cost-of-prediction-theta
  (testing "calculates the cost of predictions theta relating x and y"
    (let [exact-cost (cost x y theta)
          rounded-cost (round exact-cost)] 
      (is (= rounded-cost 100)))))

(deftest calculate-next-theta
  (testing "calculates the next set of thetas based on gradient descent"
    (is (matrices-equal? (next-theta x y theta 0.01) expected-next-theta))))

(deftest minimize-theta
  (testing "minimizes theta over the iterations given"
    (let [res (gradient-descent x y theta 0.01 100)
          {final-theta :theta history :history} res]
      (is (matrices-equal? final-theta expected-final-theta)))))
