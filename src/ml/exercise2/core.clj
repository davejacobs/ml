(ns ml.exercise2.core
  (:require [ml.exercise2.data :as d])
  (:use incanter.core
        incanter.io
        incanter.stats))

(defn g [z]
  (/ 1 (+ 1 (exp (- z)))))

(defn cost [X y theta]
  (let [m (count X)
        h (matrix-map g (mmult X theta))
        first-fn (mmult (trans (minus y)) (log h)) 
        second-fn (mmult (trans (minus 1 y)) (log (minus 1 h)))] 
    (* (/ 1.0 m) (sum (+ first-fn second-fn)))))
