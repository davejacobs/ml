(ns ml.exercise1.core
  (:use incanter.core
        incanter.io
        incanter.stats))

(defn pwd [] (System/getProperty "user.dir"))

(def data (read-dataset (str (pwd) \/ "src/ml/exercise1/ex1data2.txt")))

(def y (sel data :cols 2))
(def m (count y))

(def X 
  (let [x-first (vec (take m (repeat 1)))
        x-rest (to-matrix (sel data :cols [0 1]))]
    (bind-columns x-first x-rest)))
(def n (count (first X)))

(def theta (matrix [[0] [0] [0]]))

(def iterations 1500)
(def alpha 0.01)

(defn normalize-column [column]
  (let [stdev (sd column)
        amean (mean column)
        normalize #(/ (- % amean) stdev)]
    (if (zero? stdev)
      column
      (vec (map normalize column)))))

(defn normalize-features [X]
  (trans 
    (for [i (range n)]
      (normalize-column (to-vect (sel X :cols i))))))

(defn cost [X y theta]
  (let [difference-matrix (sq (minus (mmult X theta) y))]
    (* (/ 1 (* 2 m)) 
       (sum difference-matrix))))

(defn next-theta [X y original-theta alpha]
  (let [h (mmult X original-theta)
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
