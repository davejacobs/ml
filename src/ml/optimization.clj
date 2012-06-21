(ns ml.optimization
  (:use [incanter core stats]))

(defn h [xs thetas]
  (mmult xs thetas))

(defn gradient [xs ys thetas & args]
  (let [defaults {:hypothesis-fn h}
        options (merge defaults (apply hash-map args))
        {:keys [hypothesis-fn]} options
        m (count xs)
        hypothesis (hypothesis-fn xs thetas)
        multiplier (/ 1 m)]
    (mult multiplier (mmult (trans xs) (minus hypothesis ys)))))

(defn descend [xs ys & args]
  (let [defaults {:gradient-fn gradient
                  :cost-fn nil
                  :yield-fn nil
                  :alpha 0.01
                  :iterations 100
                  :thetas (matrix 0 (second (dim xs)) 1)}
        options (merge defaults (apply hash-map args))
        {:keys [gradient-fn cost-fn yield-fn thetas alpha iterations]} options
        next-thetas #(minus % (mult alpha (gradient-fn xs ys %)))]
    (loop [current-thetas thetas, idx 0]
      (if (< idx iterations)
        (do
          (let [new-thetas (next-thetas current-thetas)]
            (when yield-fn (yield-fn (cost-fn xs ys new-thetas)))
            (recur new-thetas (inc idx))))
        current-thetas))))
