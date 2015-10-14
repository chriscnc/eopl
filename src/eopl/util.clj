(ns eopl.util
  "A namespace for common utils")


(defn bool?
  "Tests for boolean type"
  [x]
  (or (= x false) (= x true)))
