(defproject ml/ml "0.0.1-SNAPSHOT" 
  :description "A study in machine learning, using Clojure"
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [org.clojure/math.numeric-tower "0.0.1"]
                 [clojure-csv/clojure-csv "2.0.0-alpha1"]
                 [clojure.options "0.2.2"]
                 [incanter "1.3.0"]]
  :repositories {"sonatype-oss-public"
                 "https://oss.sonatype.org/content/groups/public/"}
  :test-paths ["spec"]
  :min-lein-version "2.0.0"
  :jvm-opts ["-Djava.awt.headless=true"
             "-Xms256m" "-Xmx1024m"])
