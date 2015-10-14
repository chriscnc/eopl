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
  (testing "if-exp"
    (let [env (empty-env)
          exp {:op :if-exp
               :true-exp {:op :lit-exp
                          :datum 42}
               :false-exp {:op :lit-exp
                           :datum 24}}
          t-exp (assoc exp :test-exp {:op :lit-exp
                                      :datum true})
          f-exp (assoc exp :test-exp {:op :lit-exp
                                      :datum false})]
      (is (= 42 (eval-expression t-exp env)))
      (is (= 24 (eval-expression f-exp env)))))

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
  (testing "equal?"
    (is (apply-primitive :equal? [4 4]))
    (is (not (apply-primitive :equal? [3 4]))))
  (testing "zero?"
    (is (apply-primitive :zero? [0]))
    (is (not (apply-primitive :zero? [4]))))
  (testing "greater?"
    (is (apply-primitive :greater? [4 3]))
    (is (not (apply-primitive :greater? [3 4]))))
  (testing "less?"
    (is (apply-primitive :less? [3 4]))
    (is (not (apply-primitive :less? [4 3]))))
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


(deftest test-true-value?
  (testing "true-value?"
    (is (= true (true-value? true)))
    (is (= false (true-value? false)))))

