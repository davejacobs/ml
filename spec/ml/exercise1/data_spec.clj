(ns ml.exercise1.data-spec
  (:use ml.exercise1.data
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

(deftest data-import
  (testing "data is read properly"
           (is (= (count data) 2))
           (is (= (take 5 y) expected-y))
           (is (= (take 5 X) expected-X))))
