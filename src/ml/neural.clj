(ns ml.neural
  (:require [ml.logistic :as logistic])
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
  (matrix (reduce activate as theta-layers)))

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

;; Returns new theta-layers based on cost of hypothesis
;; Naive, ungeneralized implementation (for two layers)
(defn+opts update-thetas [xs ys [thetas-1 thetas-2]]
  ; (let [initial-deltas-1 (apply matrix (dim thetas-1))
        ; initial-deltas-2 (apply matrix (dim thetas-2))]
    ; (reduce #() [initial-deltas-1 initial-deltas-2]))
  (let [m (first (dim xs))
        multiplier (/ 1 m)
        a1 (with-bias-unit xs)
        z2 (mmult a1 (trans thetas-1))
        a2 (with-bias-unit (matrix-map logistic/g z2))
        z3 (mmult a2 (trans thetas-2))
        hypothesis (matrix (matrix-map logistic/g z3))
        d3 (minus hypothesis ys)
        biased-d2 (mult (mmult d3 thetas-2)
                        (with-bias-unit (g-prime z2)))
        d2 (without-bias-unit biased-d2)
        delta-2 (mmult (trans d3) a2)
        delta-1 (mmult (trans d2) a1)
        theta-1-grad (mult multiplier delta-1)
        theta-2-grad (mult multiplier delta-2)]
    {:cost (cost xs ys [thetas-1 thetas-2])
     :gradients [theta-1-grad theta-2-grad]}))

(defn conj-neuron [neurons weights]
  (let [inputs (-> neurons last :outputs with-bias-unit)
        values (mmult inputs (trans weights))
        outputs (matrix-map logistic/g values)]
    (conj neurons {:inputs inputs
                   :weights weights
                   :values values
                   :outputs outputs})))

(defn theta-errors [xs ys theta-layers]
  (let [m (nrow xs)
        multiplier (/ 1 m)

        ;; Implementation note: We need to keep a record of the
        ;; feed-forward process to make backpropagation efficient
        ;; -- so instead of simply reducing over the activation of
        ;; each neuron, as in cost analysis, we conj onto a list of
        ;; "neurons", which we can look back to for previous activity
        ;; levels.
        neurons (reduce conj-neuron [{:outputs xs}] theta-layers)
        hypothesis (-> neurons last :outputs matrix)

        neuron-1 (first neurons)
        neuron-2 (second neurons)

        thetas-1 (first theta-layers)
        thetas-2 (second theta-layers)

        d3 (minus hypothesis ys)
        biased-d2 (mult (mmult d3 thetas-2)
                        (with-bias-unit (g-prime (neuron-2 :values))))
        d2 (without-bias-unit biased-d2)

        delta-2 (mmult (trans d3) (with-bias-unit (neuron-2 :outputs)))
        delta-1 (mmult (trans d2) (with-bias-unit (neuron-1 :outputs)))
        theta-1-grad (mult multiplier delta-1)
        theta-2-grad (mult multiplier delta-2)]
    {:cost (cost xs ys [thetas-1 thetas-2])
     :gradients [theta-1-grad theta-2-grad]}))

(defn grad-check [xs ys theta-layers]
  (let [fns [update-thetas theta-errors]
        fn-results ((apply juxt fns) xs ys theta-layers)]
    (= (first fn-results) (second fn-results)))) 
