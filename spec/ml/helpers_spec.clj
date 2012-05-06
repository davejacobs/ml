(ns ml.helpers-spec
  (:use ml.helpers 
        incanter.core
        clojure.test
        [clojure.math.numeric-tower :only (round)]))

(deftest helper-functions
  (testing "close-to?"
    (testing "with a default value for sig figs (4)"
      (testing "returns true for identical integers"
        (is (close-to? 1 1)))
      (testing "returns false for non-identical integers"
        (is (not (close-to? 1 2))))
      (testing "returns true for identical rationals"
        (is (close-to? 1/3 1/3)))
      (testing "returns true for floats equal to at least 4 figures"
        (is (close-to? 0.00001 0.00002)))
      (testing "returns false for floats equal only to 3 figures"
        (is (not (close-to? 0.0001 0.0002)))))

    (testing "with a positive value for sig figs (2)"
      (testing "returns true for floats equal to 2 figures"
        (is (close-to? 0.001 0.002 2)))
      (testing "returns false for floats equal to 1 figure"
        (is (not (close-to? 0.01 0.02 2)))))

    (testing "with a negative value for sig figs (-1)"
      (testing "returns true for floats equal only when rounded to the nearest 10"
        (is (close-to? 5 6 -1)))
      (testing "returns false for floats equal only when rounded to the nearest 100"
        (is (not (close-to? 50 60 -1))))))

  ; matrices-equal calls close-to?, so all semantics tested above
  ; still apply
  (testing "matrices-equal?"
    (testing "returns true for equal vectors"
      (is (matrices-equal? [0.1 0.1] [0.1 0.1]))) 
    (testing "returns true for equal two-dimensional vectors"
      (is (matrices-equal? [[0.1 0.1] [0.2 0.2]]
                           [[0.1 0.1] [0.2 0.2]])))
    (testing "returns true for equal matrices"
      (is (matrices-equal? (matrix [[0.1 0.1] [0.2 0.2]])
                           (matrix [[0.1 0.1] [0.2 0.2]]))))
    (testing "returns true for an equal two-dimensional vector and matrix"
      (is (matrices-equal? [[0.1 0.1] [0.2 0.2]]
                           (matrix [[0.1 0.1] [0.2 0.2]])))))

  (testing "map-features"
    (testing "when a degree is not specified"
      (testing "returns the same matrix"
        (let [original [[1.0 2.0] [3.0 4.0]]]
          (is (matrices-equal? (map-features original) original)))))
    (testing "when a degree of 2 is specified"
      (testing "when :ignore-first is not passed as an option"
        (testing "returns the matrix with the *first two* columns permuted to the degree specified (as a stop-gap measure)"
          (let [original [[1.0 2.0]
                          [3.0 4.0]]
                          ;x1  x1^2 x1*x2 x2^2 x2
                expected [[1.0 1.0  2.0   4.0  2.0]
                          [3.0 9.0  12.0  16.0 4.0]]]
            (is (matrices-equal? (map-features original 2) expected)))))
      (testing "when :ignore-first is passed as an option"
        (testing "returns the matrix with the *second and third* columns permuted to the degree specified (as a stop-gap measure"
          (let [original [[1.0 1.0 2.0]
                          [1.0 3.0 4.0]]
                          ;x1  x1^2 x1*x2 x2^2 x2
                expected [[1.0 1.0  2.0   4.0  2.0]
                          [3.0 9.0  12.0  16.0 4.0]]]
            (is (matrices-equal? (map-features original 2 :ignore-first true) expected))))))))
