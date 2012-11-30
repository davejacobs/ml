(ns ml.neural
  (:require [ml.logistic :as logistic])
  (:use alex-and-georges.debug-repl 
        ml.helpers
        clojure.options
        [incanter core io stats]))

; (def lines (range 0 4501 500))
; (def xs (read-matrix-from-lines "neural-xs.csv" lines))
; (def ys (let [y-labels (read-matrix-from-lines "neural-ys.csv" lines)
              ; y-labels-as-indices (minus y-labels 1)]
          ; (sel (identity-matrix 10) :cols y-labels-as-indices)))

; (def theta-layers [(read-matrix-from-file "neural-thetas-1.csv")
                   ; (read-matrix-from-file "neural-thetas-2.csv")])

(defn apply-weights [as thetas]
  (mmult as (trans thetas)))

(defn activate [as thetas]
  (let [biased-as (with-bias-unit as)
        weighted (apply-weights biased-as thetas)]
    (matrix-map logistic/g weighted)))

(defn h [as theta-layers]
  (matrix (reduce activate as theta-layers)))

;; TODO: Clean up categories and make as general as possible
;; (make as few assumptions about incoming data as possible,
;; but create helpers to deal with that data before it reaches this code)
(defn+opts predict-category [as theta-layers | {categories (range)}]
  (let [category-for-max-index (comp (partial nth categories) index-of-max)
        probabilities (h as theta-layers)]
    (map category-for-max-index probabilities)))

(defn logarithmic-cost [hypothesis actual]
  (let [m (count hypothesis)
        multiplier (/ 1 m)
        if-0-fn (mult (minus actual) (log hypothesis))
        if-1-fn (mult (minus 1 actual) (log (minus 1 hypothesis)))
        sum-differences (minus if-0-fn if-1-fn)]
    (map sum sum-differences)))

(defn+opts cost [xs ys theta-layers | {cost-fn logarithmic-cost}]
  (let [hypothesis (h xs theta-layers)]
    (cost-fn hypothesis ys)))

(defn g-prime [layer]
  (let [sigmoid (matrix (matrix-map logistic/g layer))]
    (mult sigmoid (minus 1 sigmoid))))

(defn conj-neuron [neurons weights]
  (let [inputs (-> neurons last :outputs with-bias-unit)
        values (mmult inputs (trans weights))
        outputs (matrix (matrix-map logistic/g values))]
    (conj neurons {:inputs inputs
                   :weights weights
                   :values values
                   :outputs outputs})))

;; Arguments: 
;; - Errors from next layer 
;; - Weights from this layer 
;; - Values from this layer 
(defn calculate-error [errors weights values]
  (when values 
    (mult (mmult errors weights) 
          (with-bias-unit (g-prime values))))) 

(defn backprop-deltas [neurons prev-errors pos]
  (let [neuron (nth neurons pos)
        deltas (mmult (trans prev-errors) (-> neuron :outputs with-bias-unit))
        next-neurons (assoc-in neurons [pos :deltas] deltas)]
    (if (= pos 1)
      (map :deltas next-neurons)
      (let [weights (get-in neurons [pos :weights]) 
            values (get-in neurons [(dec pos) :values]) 
            next-errors (calculate-error prev-errors weights values)]
        (recur next-neurons (without-bias-unit next-errors) (dec pos)))))) 

(defn theta-gradients [xs ys theta-layers]
  (println "[cost]" (cost xs ys theta-layers))
  (let [multiplier (/ 1 (nrow xs))
        neurons (reduce conj-neuron [{:outputs xs}] theta-layers)
        hypothesis (-> neurons last :outputs matrix)
        last-errors (minus hypothesis ys)
        deltas (backprop-deltas neurons last-errors (dec (count neurons)))]
    (map (partial mult multiplier) deltas)))
