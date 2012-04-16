(ns ml.regression.logistic
  (:use incanter.core
        incanter.io
        incanter.stats))

(defn g [z]
  (/ 1 (+ 1 (exp (- z)))))

(defn h [xs thetas]
  (matrix-map g (mmult xs thetas)))

(defn cost-prime [xs ys thetas]
  (let [m (count xs)
        hypothesis (h xs thetas)
        multiplier (/ 1.0 m)]
    (mult multiplier (mmult (trans xs) (minus hypothesis ys)))))

(defn cost [xs ys thetas]
  (let [m (count xs)
        hypothesis (h xs thetas)
        first-fn (mmult (trans (minus ys)) (log hypothesis)) 
        second-fn (mmult (trans (minus 1 ys)) (log (minus 1 hypothesis)))
        multiplier (/ 1.0 m)]
    (* multiplier (minus first-fn second-fn))))

(defn next-thetas [xs ys thetas]
  (minus thetas (cost-prime xs ys thetas)))
