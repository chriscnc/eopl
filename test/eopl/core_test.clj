(ns eopl.core-test
  (:require [clojure.test :refer :all]
            [eopl.env :refer :all]
            [eopl.interp :refer :all]
            [eopl.parser :refer :all]))
            

(deftest test-comparison-operators
  (let [env (empty-env)]
    (are [expected actual] (= expected actual)
         3 (eval-expression (parse (read-string "(if (zero? 3) 2 3)")) env)
         2 (eval-expression (parse (read-string "(if (zero? 0) 2 3)")) env)
         2 (eval-expression (parse (read-string "(if (equal? 4 4) 2 3)")) env)
         3 (eval-expression (parse (read-string "(if (equal? 4 3) 2 3)")) env)
         2 (eval-expression (parse (read-string "(if (greater? 4 3) 2 3)")) env)
         3 (eval-expression (parse (read-string "(if (greater? 3 4) 2 3)")) env)
         2 (eval-expression (parse (read-string "(if (less? 3 4) 2 3)")) env)
         3 (eval-expression (parse (read-string "(if (less? 4 3) 2 3)")) env)
         )))

(deftest test-cond-exp
  (let [env (empty-env)]
    (are [expected actual] (= expected actual)
         0 (eval-expression (parse (read-string "(cond)")) env)
         42 (eval-expression (parse (read-string "(cond (1 42))")) env)
         43 (eval-expression (parse (read-string "(cond (0 42 1 43))")) env)
         0 (eval-expression (parse (read-string "(cond (0 42 0 43))")) env)
         )))
