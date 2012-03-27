(ns ml.exercise2.data-spec
  (:use ml.exercise2.data
        ml.helpers
        clojure.test
        incanter.core
        incanter.io
        incanter.stats))

(def expected-X
  (matrix [[1.0000 34.6237 78.0247]
           [1.0000 30.2867 43.8950]
           [1.0000 35.8474 72.9022]]))

(def expected-y [0 0 0])

(deftest data-import
  (testing "data is read properly"
    (is (= (count data) 2))
    (is (matrices-equal? (take 3 y) expected-y))
    (is (matrices-equal? (take 3 X) expected-X))))
