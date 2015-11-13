(ns eopl.core
  (:require [eopl.env :refer :all]
            [eopl.interp :refer :all]
            [eopl.parser :refer :all])
  (:gen-class))



(defn run 
  [exp-str]
  (eval-expression (parse (read-string exp-str)) (empty-env)))


(defn read-eval-print 
  []
  (do
    (print "eopl=> ")
    (flush)
    (try
      (let [line (read-line)]
        (when (not (empty? line))
          (println (eval-expression (parse (read-string line))
                                    (empty-env)))))
      (catch Exception e
        (println e)))
    (recur)))


(defn -main
  [& args]
  (read-eval-print))
