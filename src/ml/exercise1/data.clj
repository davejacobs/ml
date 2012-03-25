(ns ml.exercise1.data
  (:use incanter.core
        incanter.io))

(defn pwd [] (System/getProperty "user.dir"))

(def data (read-dataset (str (pwd) \/ "src/ml/exercise1/ex1data2.txt")))

(def y (sel data :cols 2))

(def X 
  (let [m (count y)
        x-first (vec (take m (repeat 1)))
        x-rest (to-matrix (sel data :cols [0 1]))]
    (bind-columns x-first x-rest)))

(def theta (matrix [[0] [0] [0]]))

(def iterations 1500)
(def alpha 0.01)
