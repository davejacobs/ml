(ns ml.regression.linear
  (:use incanter.core
        incanter.io
        incanter.stats))

(def defaults {:alpha 0.01 :iterations 1500})

(defn normalize [value mean stdev]
  (float (/ (- value mean) 
            stdev)))

(defn normalize-vector [column]
  (let [stdev (sd column)
        amean (mean column)
        normalize-value #(normalize % amean stdev)]
    (if (zero? stdev)
      column
      (vec (map normalize-value column)))))

(defn normalize-matrix [x]
  (let [n (count (first x))]
    (trans 
      (for [i (range n)]
        (normalize-vector (to-vect (sel x :cols i)))))))

(defn h [x theta]
  (mmult x theta))

; We include the multiplier (1/2) to make derivation
; easier. (Also, by including the entire function inline,
; we can partially differentiate this function instead
; of hard-coding it.
(defn mean-squared-error [matrix1 matrix2]
  (let [m (count matrix1)
        multiplier (/ 1 (* 2 m))]
    (* multiplier (sum (sq (minus matrix1 matrix2))))))

(defn mean-squared-error-prime [matrix1 matrix2]
  (minus matrix1 matrix2))

(defn cost
  ([x y theta]
   (cost x y theta mean-squared-error))
  ([x y theta f]
   (let [hypothesis (h x theta)]
     (f hypothesis y))))

(defn cost-prime
  ([x y theta]
   (cost-prime x y theta mean-squared-error-prime))
  ([x y theta f]
   (let [m (count x)
         hypothesis (h x theta)
         multiplier (/ 1 m)]
     (mult multiplier (mmult (trans x) (f hypothesis y))))))

(defn next-theta [x y theta alpha]
  (minus theta (mult alpha (cost-prime x y theta))))

(defn gradient-descent [x y original-theta alpha iterations]
  (loop [theta original-theta
         idx 0
         history []]
    (if (< idx iterations)
      (let [new-theta (next-theta x y theta alpha)
            history-entry (cost x y new-theta)]
        (recur new-theta (inc idx) (conj history history-entry)))
      {:theta theta :history history})))
