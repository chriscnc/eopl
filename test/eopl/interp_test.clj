(ns eopl.interp-test
  (:require [clojure.test :refer :all]
            [eopl.interp :refer :all]
            [eopl.env :refer :all]))


(deftest test-eval-expression 
  (testing "lit-exp"
    (let [env (empty-env)
          exp {:op :lit-exp
               :datum 42}]
      (is (= 42 (eval-expression exp env)))))
  (testing "var-exp"
    (let [env (extend-env (empty-env) {'x 6})
          exp {:op :var-exp
               :id 'x}]
      (is (= 6 (eval-expression exp env)))))
  (testing "primapp-exp"
    (let [env (empty-env)
          exp {:op :primapp-exp
               :prim :+
               :rands [{:op :lit-exp
                        :datum 4}
                       {:op :lit-exp
                        :datum 2}]}]
      (is (= 6 (eval-expression exp env)))))
  (testing "unknown expression"
    (let [env (empty-env)
          exp {:op :unknown}]
      (is (thrown? Exception (eval-expression exp env))))))


(deftest test-apply-primitive
  (testing "+"
    (is (= 6 (apply-primitive :+ [4 2]))))
  (testing "-"
    (is (= 2 (apply-primitive :- [4 2]))))
  (testing "*"
    (is (= 8 (apply-primitive :* [4 2]))))
  (testing "add1"
    (is (= 5 (apply-primitive :add1 [4]))))
  (testing "sub1"
    (is (= 3 (apply-primitive :sub1 [4]))))
  (testing "list"
    (is (= (list 1 2 3) (apply-primitive :list (list 1 2 3)))))
  (testing "list - empty"
    (is (= (list) (apply-primitive :list (list)))))
  (testing "car"
    ; args is a list
    (is (= 1 (apply-primitive :car (list (list 1 2 3))))))
  (testing "car on the empty list"
    (is (thrown? Exception (apply-primitive :car (list (list))))))
  (testing "cons"
    ; args is an element and a list
    (is (= (list 1 2 3) (apply-primitive :cons (list 1 (list 2 3))))))
  (testing "cdr"
    ; args is a list
    (is (= (list 2 3) (apply-primitive :cdr (list (list 1 2 3))))))
  (testing "cdr on the empty list"
    (is (= (list) (apply-primitive :cdr (list (list))))))
  )



(deftest test-eval-rands
  (is (= (eval-rands [{:op :lit-exp
                       :datum 4}
                      {:op :lit-exp
                       :datum 2}] 
                     (empty-env))
         (list 4 2))))
         

(deftest test-eval-program
  (testing "a-program"
    (let [pgm {:op :a-program
               :exp {:op :lit-exp
                     :datum 42}}]
      (is (= 42 (eval-program pgm))))))
