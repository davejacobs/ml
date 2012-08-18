(ns ml.neural-spec
  (:require [ml.neural :as neural])
  (:use ml.helpers 
        incanter.core
        clojure.test))

(deftest neural-regression
  (let [lines [0 500 1000 1500 2000 2500 3000 3500 4000 4500]
        xs (read-matrix-from-lines "ex3-xs.csv" lines)
        ys (read-matrix-from-lines "ex3-ys.csv" lines) 
        thetas-1 (read-matrix-from-file "ex3-thetas-1.csv")
        thetas-2 (read-matrix-from-file "ex3-thetas-2.csv")]

    (testing "predict-category"
      (let [categories (map inc (range))
            theta-layers [thetas-1 thetas-2]
            actual (neural/predict-category xs theta-layers :categories categories)
            expected ys]
        (is (matrices-equal? actual expected))))))
