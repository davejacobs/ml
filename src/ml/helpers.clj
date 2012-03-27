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
   (let [transf-fn #(round (* % (pow 10 sig-figs)))
         transf-matrix1 (matrix-map transf-fn matrix1)
         transf-matrix2 (matrix-map transf-fn matrix2)]
     (= transf-matrix1 transf-matrix2))))
