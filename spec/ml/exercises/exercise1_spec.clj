(ns ml.exercises.exercise1-spec
  (:require [ml.exercises.exercise1 :as ex1])
  (:use ml.helpers
        clojure.test
        incanter.core
        incanter.io
        incanter.stats))

(def expected-x
  (matrix [[1.0000 2104.0000 3.0000]
           [1.0000 1600.0000 3.0000]
           [1.0000 2400.0000 3.0000]]))

(def expected-y [399900 329900 369000])

(deftest data-import
  (testing "data is read properly"
    (let [{y :y x :x theta :theta iterations :iterations alpha :alpha} data]
      (is (= (count data) 5))
      (is (matrices-equal? (take 3 y) expected-y))
      (is (matrices-equal? (take 3 x) expected-x)))))
