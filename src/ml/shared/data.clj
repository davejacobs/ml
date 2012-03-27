(ns ml.shared.data
  (:use incanter.core
        incanter.io
        ml.shared.helpers))

(defn read-data [filename]
  (read-dataset (str (pwd) \/ "src/ml/" filename)))
