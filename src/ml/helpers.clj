(ns ml.helpers
  (:require [clojure.java.io :as io])
  (:use incanter.core
        incanter.io
        incanter.stats
        clojure-csv.core
        [clojure.math.numeric-tower :only (round)]))

(def ^:dynamic *sig-figs* 4)

(defn pwd [] (System/getProperty "user.dir"))

(defn close-to?
  ([val1 val2]
   (close-to? val1 val2 *sig-figs*))
  ([val1 val2 sig-figs]
   (let [transf-fn #(round (* % (pow 10 sig-figs)))
         transf-val1 (transf-fn val1)
         transf-val2 (transf-fn val2)]
   (= transf-val1 transf-val2))))

(defn matrices-equal?
  ([matrix1 matrix2] 
   (matrices-equal? matrix1 matrix2 *sig-figs*))
  ([matrix1 matrix2 sig-figs]
   (let [matrices (map matrix [matrix1 matrix2])
         transf-fn #(round (* % (pow 10 sig-figs)))
         transform-and-vectorize-fn #(matrix-map transf-fn %)
         transf-matrices (map transform-and-vectorize-fn matrices)]
     (= (first transf-matrices) (second transf-matrices)))))

(defn with-bias-unit [xs]
  (let [m (first (dim xs))
        bias-unit (vec (take m (repeat 1)))]
    (bind-columns bias-unit (if (instance? incanter.core.Dataset xs)
                              (to-matrix xs) xs))))

(defn without-bias-unit [xs]
  (sel (matrix xs) :cols (range 1 (ncol xs))))

; I haven't come up with a good algorithm for mapping features yet
; so I'm manually mapping the first two columns into derivative features
; for now.
(defn map-features 
  ([xs] xs)
  ([xs degree & remaining]
   (let [options (apply hash-map remaining)
         first-idx (if (options :ignore-first) 1 0)
         second-idx (inc first-idx)
         all (matrix xs)
         one (sel all :cols first-idx)
         two (sel all :cols second-idx)
         derived (for [i (range 1 (inc degree)) j (range (inc i))]
                   (mult (pow one (- i j)) (pow two j)))
         fixed-orientation (trans (matrix derived))]
     (if (options :ignore-first)
       (with-bias-unit fixed-orientation)
       fixed-orientation))))

(defn map-hash [f coll]
  (let [transform (fn [[k v]] [k (f v)])]
    (apply hash-map (map transform coll))))

(defn map-hash-2 [f coll]
  (let [transform (fn [[k v]] [k (f v)])]
    (map transform coll)))

(def map-dims
  (partial map-hash-2 dim))

(defn random-matrix [[r c]]
  (matrix (rand 1) r c))

(defn multi-nth [values indices]
  (let [indices-set (set indices)
        filter-fn #(when (contains? indices-set %1) %2)]
    (keep-indexed filter-fn values)))

(defn full-filename [filename] (str (pwd) "/spec/ml/data/" filename))

(defn parse-csv-line [line]
  (->> line parse-csv flatten (map read-string)))

(defn read-dataset-from-file [filename]
  (read-dataset (full-filename filename)))

(def read-matrix-from-file (comp to-matrix read-dataset-from-file))

(defn read-matrix-from-lines [filename indices]
  (with-open [r (io/reader (full-filename filename))]
    (->> indices
      (multi-nth (line-seq r))
      (map parse-csv-line)
      doall
      matrix)))

(defn index-of [value values]
  (first (keep-indexed #(if (= value %2) %1) values)))

(defn index-of-max [values]
  (index-of (apply max values) values))

(defn msample 
  ([m] (msample m 3))
  ([m size]
   (when m 
     (sel (matrix m)
          :cols (range size) 
          :rows (range size)))))

(defn unroll [m]
  (flatten m))

(defn roll [flattened-m & dims] [])
