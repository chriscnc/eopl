(ns eopl.env-test
  (:require [clojure.test :refer :all]
            [eopl.env :refer :all])
  (:import [eopl.env EmptyEnv ExtendedEnv]))


(deftest env-functions
  (testing "empty environment"
    (is (= (EmptyEnv.) 
           (empty-env))))
  (testing "simple extension"
    (is (= (ExtendedEnv. ['a] [42] (EmptyEnv.))
           (extend-env ['a] [42] (empty-env)))))
  (testing "lookup"
    (let [e1 (extend-env ['a 'b] [42 24] (empty-env)) 
          e2 (extend-env ['a] [32] e1)]
      (testing "in current scope"
        (is (= (apply-env e1 'a) 42)))
      (testing "with shadowing"
        (is (= (apply-env e2 'a) 32)))
      (testing "in outer scope"
        (is (= (apply-env e2 'b) 24)))
      (testing "not found"
        (is (thrown? Exception (apply-env e2 'c))))
      )))



