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
    {:xs xs, :ys ys, :m m, :iterations 100, :alpha 0.01}))

(deftest logistic-regression
  (let [{:keys [xs ys m alpha iterations]} data]
    (testing "data import"
      (let [{:keys [xs ys m]} data
            expected-xs [[1.0 34.6237 78.0247]
                         [1.0 30.2867 43.8950]
                         [1.0 35.8474 72.9022]
                         [1.0 60.1826 86.3086]]
            expected-ys [0 0 0 1]]
        (is (= m 100))
        (is (matrices-equal? (take 4 xs) expected-xs))
        (is (matrices-equal? (take 4 ys) expected-ys))))

    (testing "g"
      (testing "calculates the sigmoid of a scalar"
        (is (close-to? (logistic/g 1) 0.73105))))

    (testing "h"
      (testing "performs the matrix product of data"
        (let [xs [[0] [0]]
              thetas [[0 0]]]
          (= (logistic/h xs thetas) (mmult xs thetas))))
      (testing "returns a scalar when the matrix product is 1 x 1"
        (= (class (logistic/h [[0] [0]] [[0 0]])) java.lang.Double))
      (testing "returns a matrix when the matrix product is m x 1 or 1 x n"
        (= (class (logistic/h [0] [[0 0]])) incanter.Matrix))
      (testing "returns a matrix when the matrix product is m x n"
        (= (class (logistic/h [[0 0]] [[0] [0]])) incanter.Matrix)))

    (testing "cost-prime"
      (testing "calculates the first cost gradient"
        (let [cost-prime (logistic/cost-prime xs ys [[0] [0] [0]])
              expected [[-0.1000] [-12.0092] [-11.2628]]]
          (is (matrices-equal? cost-prime expected)))))

    (testing "next-thetas"
      (testing "calculates the first thetas using alpha"
        (let [next-thetas (logistic/next-thetas xs ys [[0] [0] [0]] alpha)
              expected [[0.0010] [0.1201] [0.1126]]]
          (is (matrices-equal? next-thetas expected)))))

    (testing "cost"
      (testing "calculates the cost of thetas relating xs and ys"
        (let [first-cost (logistic/cost xs ys [[0] [0] [0]])]
          (is (close-to? first-cost 0.693147))))
      (testing "calculates the regularized cost of thetas"
        (let [lambda 0.1
              regularized-cost (logistic/cost xs ys [[0] [0] [0]] lambda)]
          (is (close-to? regularized-cost 0.0)))))

    (testing "gradient-descent"
      (testing "minimizes thetas"
        (testing "yields history and final thetas after given iterations"
          (let [descent (logistic/gradient-descent xs ys alpha iterations)
                {last-thetas :thetas history :history} descent
                expected [[0] [0] [0]]]
            (is (matrices-equal? last-thetas expected))))))

    (testing "probabilities"
      (testing "predicts a probability of points being in a category given thetas"
        (let [points [[1 45 85]]
              category 1
              probability (logistic/probabilities points [[0] [0] [0]] category)]
          (is (close-to? probability 0.776289)))))

    (testing "predict-category"
      (testing "predicts the category of points given thetas"
        (let [points [[1 45 85]]
              threshold 0.5
              category (logistic/predict-category points [[0] [0] [0]] threshold)]
          (is (close-to? category 1)))))))
