(ns ml.exercise1.core-spec
  (:use ml.exercise1.core
        clojure.test
        incanter.core
        incanter.io
        incanter.stats))

(def expected-X
  (matrix [[1.0000 2104.0000 3.0000]
           [1.0000 1600.0000 3.0000]
           [1.0000 2400.0000 3.0000]
           [1.0000 1416.0000 2.0000]
           [1.0000 3000.0000 4.0000]]))

(def expected-y [399900 329900 369000 232000 539900])

(def expected-normalized-X
  (matrix [[1.0000  0.1300 -0.2237]
           [1.0000 -0.5042 -0.2237]
           [1.0000  0.5025 -0.2237]
           [1.0000 -0.7357 -1.5378]
           [1.0000  1.2575  1.0904]]))

; Test data import, then use expected-X for remaining tests
(deftest data-import
  (testing "data is read properly"
           (is (= (count data) 2))
           (is (= m 47))
           (is (= (take 5 y) expected-y))
           (is (= (take 5 X) expected-X))))

; Not yet passing because of class type differences
(deftest normalize-features-of-X
  (testing "properly scales all features"
           (let [X expected-X
                 normalized-X (normalize-features X)
                 head-normalized-X (take 5 normalized-X)]
             (is (= head-normalized-X expected-normalized-X)))))

(run-tests)
