(ns ml.regression.linear
  (:use incanter.core
        incanter.io
        incanter.stats))

(def defaults {:alpha 0.01 :iterations 1500})

(defn normalize [value mean stdev]
  (if (nil? value)
    nil
    (float (/ (- value mean) 
              stdev))))

(defn normalize-vector [column]
  (let [stdev (sd column)
        amean (mean column)
        normalize-value #(normalize % amean stdev)]
    (if (zero? stdev)
      column
      (vec (map normalize-value column)))))

(defn normalize-matrix [xs]
  (let [n (count (first xs))]
    (trans 
      (for [i (range n)]
        (normalize-vector (to-vect (sel xs :cols i)))))))

; This could also be (def h mmult), but I don't think that
; would be as clear.
(defn h [xs thetas]
  (mmult xs thetas))

; We include the multiplier (1/2) to make derivation
; easier. (Also, by including the entire function inline,
; we can (eventually) partially differentiate this function instead
; of hard-coding it.
(defn mean-squared-error [matrix1 matrix2]
  (let [m (count matrix1)
        multiplier (/ 1 (* 2 m))]
    (* multiplier (sum (sq (minus matrix1 matrix2))))))

(defn cost
  ([xs ys thetas]
   (cost xs ys thetas mean-squared-error))
  ([xs ys thetas f]
   (let [hypothesis (h xs thetas)]
     (f hypothesis ys))))

(defn cost-prime
  [xs ys thetas]
  (let [m (count xs)
        hypothesis (h xs thetas)
        multiplier (/ 1 m)]
    (mult multiplier (mmult (trans xs) (minus hypothesis ys)))))

(defn next-thetas [xs ys thetas alpha]
  (minus thetas (mult alpha (cost-prime xs ys thetas))))

(defn gradient-descent 
  ([xs ys]
   (let [{:keys [alpha iterations]} defaults]
     (gradient-descent xs ys alpha iterations)))
  ([xs ys alpha iterations]
   (let [original-thetas (matrix 0 (second (dim xs)) 1)]
     (gradient-descent xs ys alpha iterations original-thetas)))
  ([xs ys alpha iterations original-thetas]
   (loop [thetas original-thetas
          idx 0
          history []]
     (if (< idx iterations)
       (let [new-thetas (next-thetas xs ys thetas alpha)
             history-entry (cost xs ys new-thetas)]
         (recur new-thetas (inc idx) (conj history history-entry)))
       {:thetas thetas :history history}))))
