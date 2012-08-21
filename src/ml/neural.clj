(ns ml.neural
  (:use ml.helpers
        clojure.options
        [incanter core io stats]))

(defn g [z]
  (/ 1 (+ 1 (exp (- z)))))

(defn apply-weights [as thetas]
  (mmult as (trans thetas)))

(defn activate [as thetas]
  (let [biased-as (with-bias-unit as)
        weighted (apply-weights biased-as thetas)]
    (matrix-map g weighted)))

(defn h [as theta-layers]
  (reduce activate as theta-layers))

(defn+opts predict-category [as theta-layers | {categories (range)}]
  (let [category-for-max-index (comp (partial nth categories) index-of-max)
        probabilities (h as theta-layers)]
    (map category-for-max-index probabilities)))

(defn logarithmic-cost [xs ys theta-layers]
  (let [m (count xs)
        multiplier (/ 1 m)
        hypothesis (matrix (h xs theta-layers))
        if-0-fn (mult (minus ys) (log hypothesis))
        if-1-fn (mult (minus 1 ys) (log (minus 1 hypothesis)))
        sum-differences (minus if-0-fn if-1-fn)]
    (map sum sum-differences)))

(defn+opts cost [xs ys theta-layers | {cost-fn logarithmic-cost}]
  (cost-fn xs ys theta-layers))
