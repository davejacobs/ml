(ns ml.helpers-spec
  (:require [ml.helpers :as h])
  (:use incanter.core
        clojure.test
        [clojure.math.numeric-tower :only (round)]))

(deftest helper-functions
  (testing "close-to?"
    (testing "with a default value for sig figs (4)"
      (testing "returns true for identical integers"
        (is (h/close-to? 1 1)))
      (testing "returns false for non-identical integers"
        (is (not (h/close-to? 1 2))))
      (testing "returns true for identical rationals"
        (is (h/close-to? 1/3 1/3)))
      (testing "returns true for floats equal to at least 4 figures"
        (is (h/close-to? 0.00001 0.00002)))
      (testing "returns false for floats equal only to 3 figures"
        (is (not (h/close-to? 0.0001 0.0002)))))

    (testing "with a positive value for sig figs (2)"
      (testing "returns true for floats equal to 2 figures"
        (is (h/close-to? 0.001 0.002 2)))
      (testing "returns false for floats equal to 1 figure"
        (is (not (h/close-to? 0.01 0.02 2)))))

    (testing "with a negative value for sig figs (-1)"
      (testing "returns true for floats equal only when rounded to the nearest 10"
        (is (h/close-to? 5 6 -1)))
      (testing "returns false for floats equal only when rounded to the nearest 100"
        (is (not (h/close-to? 50 60 -1))))))

  ; matrices-equal calls close-to?, so all semantics tested above
  ; still apply
  (testing "matrices-equal?"
    (testing "returns true for equal vectors"
      (is (h/matrices-equal? [0.1 0.1] [0.1 0.1]))) 
    (testing "returns true for equal two-dimensional vectors"
      (is (h/matrices-equal? [[0.1 0.1] [0.2 0.2]]
                           [[0.1 0.1] [0.2 0.2]])))
    (testing "returns true for equal matrices"
      (is (h/matrices-equal? (matrix [[0.1 0.1] [0.2 0.2]])
                           (matrix [[0.1 0.1] [0.2 0.2]]))))
    (testing "returns true for an equal two-dimensional vector and matrix"
      (is (h/matrices-equal? [[0.1 0.1] [0.2 0.2]]
                           (matrix [[0.1 0.1] [0.2 0.2]]))))
    (testing "returns false for non-equal vectors"
      (is (not (h/matrices-equal? [1 2] [2 1])))))

  (testing "with-bias-unit"
    (testing "for a one-dimentional vector"
      (testing "returns a matrix with the bias unit as the first column"
        (is (h/matrices-equal? (h/with-bias-unit [0 1 2]) [[1 0] [1 1] [1 2]]))))
    (testing "for a matrix"
      (testing "returns a matrix with the bias unit as the first column"
        (is (h/matrices-equal? (h/with-bias-unit [[0 1] [2 3]]) [[1 0 1] [1 2 3]])))))

  (testing "without-bias-unit"
    (testing "for a matrix"
      (testing "returns a matrix with the bias unit column removed"
        (is (h/matrices-equal? (h/without-bias-unit [[1 0 1] [1 2 3]])
                               [[0 1] [2 3]])))))

  (testing "map-features"
    (testing "when a degree is not specified"
      (testing "returns the same matrix"
        (let [original [[1.0 2.0] [3.0 4.0]]]
          (is (h/matrices-equal? (h/map-features original) original)))))
    (testing "when a degree of 2 is specified"
      (testing "when :ignore-first is not passed as an option"
        (testing "returns the matrix with the *first two* columns permuted to the degree specified (as a stop-gap measure)"
          (let [original [[1.0 2.0]
                          [3.0 4.0]]
                         ; x1  x2  x1^2 x1*x2 x2^2
                expected [[1.0 2.0 1.0  2.0   4.0]
                          [3.0 4.0 9.0  12.0  16.0]]]
            (is (h/matrices-equal? (h/map-features original 2) expected)))))
      (testing "when :ignore-first is passed as an option"
        (testing "returns the matrix with the *second and third* columns permuted to the degree specified (as a stop-gap measure"
          (let [original [[1.0 1.0 2.0]
                          [1.0 3.0 4.0]]
                         ; 1.0 x1  x2  x1^2 x1*x2 x2^2
                expected [[1.0 1.0 2.0 1.0  2.0   4.0]
                          [1.0 3.0 4.0 9.0  12.0  16.0]]]
            (is (h/matrices-equal? (h/map-features original 2 :ignore-first true) expected))))))
    (testing "when a degree of 3 is specified"
      (testing "when :ignore-first is not passed as an option"
        (testing "returns the matrix with the *first two* columns permuted to the degree specified (as a stop-gap measure)"
          (let [original [[1.0 2.0]
                          [3.0 4.0]]
                         ; x1  x2  x1^2 x1*x2 x2^2 x1^3 x1^2*x2 x1*x2^2 x2^3
                expected [[1.0 2.0 1.0  2.0   4.0  1.0  2.0     4.0     8.0]
                          [3.0 4.0 9.0  12.0  16.0 27.0 36.0    48.0    64.0]]]
            (is (h/matrices-equal? (h/map-features original 3) expected)))))))
  
  (testing "index-of"
    (testing "returns the value's index when the value is in the collection"
      (is (= (h/index-of 2 [1 2 3]) 1)))
    (testing "returns nil when the value is not in the collection"
      (is (= (h/index-of "not-present" [1 2 3]) nil))))
  
  (testing "index-of-max"
    (testing "returns the index of the greatest value in the collection"
      (is (= (h/index-of-max [-25 0 25]) 2)))
    (testing "returns the first index of the greatest value if duplicated"
      (is (= (h/index-of-max [-25 0 25 25]) 2))))
  
  (testing "multi-nth"
    (testing "returns mutliple nth values in a collection (lazily)"
      (is (= (h/multi-nth [1 2 3] [1 2]) [2 3]))))
  
  (testing "parse-csv-line"
    (testing "returns a vector with values parsed by read-string"
      (is (= (h/parse-csv-line "1,2,3") [1 2 3])))))
