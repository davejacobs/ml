(ns ml.knn
  (:use incanter.core
        incanter.io
        incanter.stats))

(defn classify [xs ys point k]
  (let [m (first (dim xs))
        points (matrix (take m (repeat point)))]
    (->> points
      (minus xs)
      sq
      (map sum)
      (map sqrt)
      (bind-columns ys)
      to-vect
      (sort-by last)
      (take k)
      (map first)
      (partition-by identity)
      (map (juxt first count))
      (max-key last)
      ffirst)))
