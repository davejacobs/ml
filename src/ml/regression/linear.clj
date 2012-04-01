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

(defn normalize-matrix [X]
  (let [n (count (first X))]
    (trans 
      (for [i (range n)]
        (normalize-vector (to-vect (sel X :cols i)))))))

(defn cost [X y theta]
  (let [m (count y)
        difference-matrix (sq (minus (mmult X theta) y))]
    (* (/ 1 (* 2 m)) 
       (sum difference-matrix))))

(defn next-theta [X y original-theta alpha]
  (let [m (count y)
        h (mmult X original-theta)
        diff (trans (minus h y))
        prod (mmult diff X)]
    (minus original-theta (trans (mult alpha (/ 1 m) prod)))))

(defn gradient-descent [X y original-theta alpha iterations]
  (loop [theta original-theta
         idx 0
         history []]
    (if (< idx iterations)
      (let [new-theta (next-theta X y theta alpha)
            history-entry (cost X y new-theta)]
        (recur new-theta (inc idx) (conj history history-entry)))
      {:theta theta :history history})))
