(ns ml.regression.logistic
  (:use incanter.core
        incanter.io
        incanter.stats))

(defn g [z]
  (/ 1 (+ 1 (exp (- z)))))

(defn cost [x y theta]
  (let [m (count x)
        h (matrix-map g (mmult x theta))
        first-fn (mmult (trans (minus y)) (log h)) 
        second-fn (mmult (trans (minus 1 y)) (log (minus 1 h)))]
    (* (/ 1.0 m) (+ first-fn second-fn))))
