(ns ml.regression.logistic-spec
  (:require [ml.regression.logistic :as logistic])
  (:use ml.helpers 
        incanter.core
        clojure.test
        [clojure.math.numeric-tower :only (round)]))
