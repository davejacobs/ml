(ns ml.helpers
  (:use incanter.core
        incanter.io
        [clojure.math.numeric-tower :only (round)]))

(def ^:dynamic *sig-figs* 4)

(defn pwd [] (System/getProperty "user.dir"))

(defn read-data [filename]
  (read-dataset (str (pwd) "/src/ml/" filename)))

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
         two (sel all :cols second-idx)]
       (trans (matrix [one, (pow one 2), (mult one two), (pow two 2), two])))))
