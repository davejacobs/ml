(ns ml.neural-spec
  (:require [ml.neural :as neural])
  (:use ml.helpers 
        incanter.core
        clojure.test))

(deftest neural-regression
  (let [lines [0 500 1000 1500 2000 2500 3000 3500 4000 4500]
        xs (read-matrix-from-lines "ex3-xs.csv" lines)
        y-labels (read-matrix-from-lines "ex3-ys.csv" lines) 
        y-labels-as-indices (minus y-labels 1)
        ys (sel (identity-matrix 10) :cols y-labels-as-indices)
        thetas-1 (read-matrix-from-file "ex3-thetas-1.csv")
        thetas-2 (read-matrix-from-file "ex3-thetas-2.csv")]

    (testing "predict-category"
      (let [categories (map inc (range))
            theta-layers [thetas-1 thetas-2]
            actual (neural/predict-category xs theta-layers :categories categories)
            expected y-labels]
        (is (matrices-equal? actual expected))))
    
    (testing "cost"
      (let [actual (neural/cost xs ys [thetas-1 thetas-2])
            expected [0.0558 0.1652 0.0423 0.1612 0.4744 0.0558 0.2726 0.1059 0.118 0.0345]]
        (is (matrices-equal? actual expected))))))
