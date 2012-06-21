(ns ml.regression.logistic-spec
  (:require [ml.regression.logistic :as logistic])
  (:use ml.helpers 
        incanter.core
        clojure.test
        [clojure.math.numeric-tower :only (round)]))

(deftest logistic-regression
  (let [raw-data (read-data "data/ex2.2")
        m (first (dim raw-data))
        xs (with-bias-unit (sel raw-data :cols [0 1]))
        ys (sel raw-data :cols 2)
        iterations 100
        alpha 0.01]
    (testing "data import"
      (let [expected-xs [[1.0000  0.0513 0.6996]
                         [1.0000 -0.0927 0.6849]
                         [1.0000 -0.2137 0.6923]
                         [1.0000 -0.3750 0.5022]]
            expected-ys [1 1 1 1]]
        (is (= m 118))
        (is (matrices-equal? (take 4 xs) expected-xs))
        (is (matrices-equal? (take 4 ys) expected-ys))))

    (testing "g"
      (testing "calculates the sigmoid of a scalar"
        (is (close-to? (logistic/g 1) 0.73105))))

    (testing "h"
      (testing "performs the matrix product of data"
        (let [xs [[0] [1]]
              thetas [[2 3]]]
          (= (logistic/h xs thetas) (mmult xs thetas))))
      (testing "returns a scalar when the matrix product is 1 x 1"
        (= (class (logistic/h [[0] [0]] [[0 0]])) java.lang.Double))
      (testing "returns a matrix when the matrix product is m x 1 or 1 x n"
        (= (class (logistic/h [0] [[0 0]])) incanter.Matrix))
      (testing "returns a matrix when the matrix product is m x n"
        (= (class (logistic/h [[0 0]] [[0] [0]])) incanter.Matrix)))

    (testing "gradient"
      (testing "calculates the cost gradient"
        (let [gradient (logistic/gradient xs ys [[0] [0] [0]])
              expected [[0.0085] [0.0188] [0.0001]]]
          (is (matrices-equal? gradient expected)))))

    (testing "cost"
      (testing "calculates the cost of thetas that predict ys using xs"
        (let [first-cost (logistic/cost xs ys [[0] [0] [0]])]
          (is (close-to? first-cost 0.693147))))
      (testing "calculates the regularized cost of thetas"
        (let [lambda 0.01
              degree 6
              augmented-xs (map-features xs degree :ignore-first true) 
              thetas (matrix 0.5 (second (dim augmented-xs)) 1)
              regularized-cost (logistic/cost augmented-xs ys thetas :lambda lambda)]
          (is (close-to? regularized-cost 1.2318 3)))))

    (testing "probabilities"
      (testing "predicts the probability of a single point being in a category given thetas"
        (let [points [[1 0.5 2]]
              thetas [[0] [0.5] [1]]
              category 1
              probability (logistic/probabilities points thetas category)]
          (is (close-to? probability 0.9047))))
      (testing "predicts the probability of a multiple points being in a category given thetas"
        (let [points [[1 0.5 2] 
                      [1 0.75 2]]
              thetas [[0] [0.5] [1]]
              category 1
              probabilities (logistic/probabilities points thetas category)]
          (is (matrices-equal? probabilities [0.9047 0.9149])))))

    (testing "predict-category"
      (testing "predicts the category of points given thetas"
        (let [points [[1 0.5 2]]
              thetas [[0] [0.5] [1]]
              threshold 0.5
              category (logistic/predict-category points thetas threshold)]
          (is (close-to? category 1)))))))
