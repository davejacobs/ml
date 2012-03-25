(defproject ml "0.0.1-SNAPSHOT"
  :description "A study in machine learning, using Clojure"
  :jvm-opts ["-D java.awt.headless=true"]
  :repositories {"sonatype-oss-public"
                 "https://oss.sonatype.org/content/groups/public/"}
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [org.clojure/math.numeric-tower "0.0.1"]
                 [incanter "1.3.0-SNAPSHOT"]]
  :test-path "spec")
