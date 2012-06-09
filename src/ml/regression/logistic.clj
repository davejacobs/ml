(ns ml.regression.logistic
  (:use incanter.core
        incanter.io
        incanter.stats))

(def defaults {:alpha 0.01 :iterations 1500})

(defn g [z]
  (/ 1 (+ 1 (exp (- z)))))

(defn h [xs thetas]
  (matrix-map g (mmult xs thetas)))

(defn cost-prime [xs ys thetas]
  (let [m (count xs)
        hypothesis (h xs thetas)
        multiplier (/ 1.0 m)]
    (mult multiplier (mmult (trans xs) (minus hypothesis ys)))))

(defn cost 
  ([xs ys thetas] 
   (cost xs ys thetas 1))
  ([xs ys thetas lambda] 
   (let [m (count xs)
         hypothesis (h xs thetas)
         first-fn (mmult (trans (minus ys)) (log hypothesis)) 
         second-fn (mmult (trans (minus 1 ys)) (log (minus 1 hypothesis)))
         multiplier (/ 1.0 m)
         sum-differences (minus first-fn second-fn)
         squared-sum (sum (sq (rest thetas)))
         regularization (* (/ lambda (* 2 m)) squared-sum)
         differences (mult multiplier sum-differences)]
     (plus differences regularization))))

(defn next-thetas [xs ys thetas alpha]
  (minus (matrix thetas) (mult alpha (cost-prime xs ys thetas))))

; This is copied from ml.regression.linear. I will dry this up
; as soon as specs are green
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
       {:thetas thetas, :history history}))))

(defn probabilities [points thetas category]
  (let [probabilities-of-one (h points thetas)]
    (if (zero? category)
      (minus 1 probabilities-of-one)
      probabilities-of-one)))

(defn predict-category [points thetas threshold]
  (let [prediction-fn #(if (> % threshold) 1 0)]
    (matrix-map prediction-fn (probabilities points thetas 1))))
