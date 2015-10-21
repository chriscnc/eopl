(ns eopl.core
  (:require [eopl.env :refer :all]
            [eopl.interp :refer :all]
            [eopl.parser :refer :all])
  (:gen-class))


(defn read-eval-print 
  []
  (let [env (extend-env (empty-env) {'emptylist (list)})]
    (do
      (print "eopl=> ")
      (flush)
      (try
        (let [line (read-line)]
          (when (not (empty? line))
            (println (eval-expression (parse (read-string line)) env))))
        (catch Exception e
          (println e)))
      (recur))))


(defn -main
  [& args]
  (read-eval-print))
