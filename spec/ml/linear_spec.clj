(ns ml.linear-spec
  (:require [ml.linear :as linear])
  (:use ml.helpers
        incanter.core
        clojure.test
        [clojure.math.numeric-tower :only (round)]))

(deftest linear-regression
  (let [raw-data (read-dataset-from-file "ex1.2.csv")
        m (first (dim raw-data))
        xs (with-bias-unit (sel raw-data :cols [0 1]))
        ys (sel raw-data :cols 2)
        iterations 100
        alpha 0.01
        normalized-xs (linear/normalize-matrix xs)]
    (testing "data is read properly"
      (let [expected [[1.0 2104.0 3.0]
                      [1.0 1600.0 3.0]
                      [1.0 2400.0 3.0]
                      [1.0 1416.0 2.0]]]
        (is (= m 47))
        (is (matrices-equal? (take 4 xs) expected))
        (is (matrices-equal? (take 4 ys) [399900 329900 369000 232000]))))

    (testing "normalize"
      (testing "passes through nil"
        (is (= (linear/normalize nil nil nil) nil)))
      (testing "properly scales one value via mean and standard deviation"
        (is (= (linear/normalize 1 2 1) -1.0)))) 

    (testing "normalize-vector"
      (testing "passes through an empty vector"
        (is (= (linear/normalize-vector []) [])))
      (testing "properly scales all values in a column"
        (let [normalized-vector (linear/normalize-vector [1 2 3])]
          (is (= normalized-vector [-1.0 0.0 1.0])))))

    (testing "normalize-xs"
      (testing "properly scales all features"
        (let [expected [[1.0000 0.1300 -0.2237]
                        [1.0000 -0.5042 -0.2237]
                        [1.0000 0.5025 -0.2237]
                        [1.0000 -0.7357 -1.5378]]]
          (is (matrices-equal? (take 4 (linear/normalize-matrix xs)) expected)))))

    (testing "cost"
      (testing "calculates the cost of weights (thetas) relating xs and ys"
        (let [thetas (matrix [[0] [0] [0]])
              exact-cost (linear/cost normalized-xs ys thetas)
              rounded-cost (round exact-cost)] 
          (is (close-to? rounded-cost 65591548106 -1))))
      
      (testing "accepts a custom cost function"
        (let [thetas (matrix [[0] [0] [0]])
              cost-fn #(sum (minus %2 %1))
              actual-cost (linear/cost normalized-xs ys thetas :cost-fn cost-fn)]
          (is (close-to? actual-cost 15999395)))))
    
    (testing "descend"
      (let [expected [[215810.6168] [61384.0308] [20273.5507]]
            last-thetas (linear/descend normalized-xs ys)]
        (is (matrices-equal? last-thetas expected))))))
