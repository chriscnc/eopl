(ns eopl.env-test
  (:require [clojure.test :refer :all]
            [eopl.env :refer :all]))

(deftest env-functions
  (testing "empty environment"
    (is (= (empty-env) '())))
  (testing "simple extension"
    (is (= (extend-env (empty-env) {'a 42}) 
           (list {'a 42}))))
  (let [e1 (extend-env (empty-env) {'a 42 'b 24}) 
        e2 (extend-env e1 {'a 32})]
    (testing "lookup"
      (testing "in current scope"
        (is (= (apply-env e1 'a) 42)))
      (testing "with shadowing"
        (is (= (apply-env e2 'a) 32)))
      (testing "in outer scope"
        (is (= (apply-env e2 'b) 24)))
      (testing "not found"
        (is (thrown? Exception (apply-env e2 'c))))
      )))



