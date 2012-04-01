(ns ml.exercises.exercise2-spec 
  (:require [ml.exercises.exercise2 :as ex2])
  (:use ml.helpers
        clojure.test
        incanter.core
        incanter.io
        incanter.stats))

(def expected-x
  (matrix [[1.0000 34.6237 78.0247]
           [1.0000 30.2867 43.8950]
           [1.0000 35.8474 72.9022]]))

(def expected-y [0 0 0])

(deftest data-import
  (testing "data is read properly"
    (let [{:keys [x y theta iterations alpha]} ex2/data]
      (is (= (count x) 100))
      (is (matrices-equal? (take 3 y) expected-y))
      (is (matrices-equal? (take 3 x) expected-x)))))
