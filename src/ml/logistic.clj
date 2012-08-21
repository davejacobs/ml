(ns ml.logistic
  (:require [ml.optimization :as optimization])
  (:use clojure.options
        [incanter core io stats]))

(defn g [z]
  (/ 1 (+ 1 (exp (- z)))))

(defn h [xs thetas]
  (matrix-map g (mmult xs thetas)))

(defn logarithmic-cost [hypothesis actual]
  (let [m (count hypothesis)
        multiplier (/ 1 m)
        if-0-fn (mmult (trans (minus actual)) (log hypothesis)) 
        if-1-fn (mmult (trans (minus 1 actual)) (log (minus 1 hypothesis)))
        sum-differences (minus if-0-fn if-1-fn)]
    (mult multiplier sum-differences)))

(defn regularize [thetas lambda]
  (let [m (count thetas)
        squared-sum (sum (sq (rest thetas)))]
    (* (/ lambda (* 2 m)) squared-sum)))

(defn+opts cost [xs ys thetas | {cost-fn logarithmic-cost}]
  (let [hypothesis (h xs thetas)]
    (cost-fn hypothesis ys)))

(defn gradient [xs ys thetas & args]
  (apply optimization/gradient xs ys thetas :hypothesis-fn h args))

(defn descend [xs ys & args]
  (apply optimization/descend xs ys :cost-fn cost args))

(defn probabilities [points thetas category]
  (let [probabilities-of-one (h points thetas)]
    (if (zero? category)
      (minus 1 probabilities-of-one)
      probabilities-of-one)))

(defn predict-category [points thetas threshold]
  (let [prediction-fn #(if (> % threshold) 1 0)]
    (matrix-map prediction-fn (probabilities points thetas 1))))
