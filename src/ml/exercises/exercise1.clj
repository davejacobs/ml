(ns ml.exercises.exercise1
  (:require [ml.regression.linear :as linear])
  (:use ml.helpers
        incanter.core
        incanter.io))

(def data 
  (let [raw-data (read-data "data/ex1.2")
        y (sel raw-data :cols 2)
        m (count y)
        x (let [x-first (vec (take m (repeat 1)))
                x-rest (to-matrix (sel raw-data :cols [0 1]))]
            (bind-columns x-first x-rest))
        theta (matrix [[0] [0] [0]])
        iterations 1500
        alpha 0.01]
    {:y y
     :x x 
     :theta theta 
     :iterations iterations 
     :alpha alpha}))

(let [{y :y x :x theta :theta iterations :iterations alpha :alpha} data]
  (println "Cost of initial theta" (data :theta) ": "
           (linear/cost x y theta)))
