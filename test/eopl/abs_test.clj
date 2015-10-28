(ns eopl.abs-test
  (:require [clojure.test :refer :all]
            [eopl.abs :refer :all]))


(deftest parse-unparse
  (let [exp '((lambda (a) (a b)) c)]
    (is (= exp (unparse-expr (parse-expr exp))))))

