(ns ml.linear
  (:require [ml.optimization :as optimization])
  (:use clojure.options 
        [incanter core io stats]))

(defn normalize [value mean stdev]
  (when value (float (/ (- value mean) stdev))))

(defn normalize-vector [column]
  (let [stdev (sd column)
        amean (mean column)
        normalize-value #(normalize % amean stdev)]
    (if (zero? stdev)
      column
      (vec (map normalize-value column)))))

(defn normalize-matrix [raw-matrix]
  (let [n (second (dim raw-matrix))]
    (trans 
      (for [i (range n)]
        (normalize-vector (to-vect (sel raw-matrix :cols i)))))))

(defn h [xs thetas]
  (mmult xs thetas))

(defn mean-squared-cost [hypothesis actual]
  (let [m (count hypothesis)
        multiplier (/ 1 (* 2 m))]
    (* multiplier (sum (sq (minus hypothesis actual))))))

(defn+opts cost [xs ys thetas | {cost-fn mean-squared-cost}]
  (let [hypothesis (h xs thetas)]
      (cost-fn hypothesis ys)))

(def gradient optimization/gradient)

(defn descend [xs ys & args]
  (apply optimization/descend xs ys :cost-fn cost args))
