(ns ml.helpers-spec
  (:use ml.helpers 
        incanter.core
        clojure.test
        [clojure.math.numeric-tower :only (round)]))

(deftest close-to-value
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
