(ns eopl.interp-test
  (:require [clojure.test :refer :all]
            [eopl.interp :refer :all]
            [eopl.env :refer :all])
  (:import [eopl.env EmptyEnv ExtendedEnv]))


(deftest test-eval-expression 
  (testing "lit-exp"
    (let [env (empty-env)
          exp {:op :lit-exp
               :datum 42}]
      (is (= 42 (eval-expression exp env)))))
  (testing "var-exp"
    (let [env (extend-env ['x] [6] (empty-env))
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
                                      :datum 1})
          f-exp (assoc exp :test-exp {:op :lit-exp
                                      :datum 0})]
      (is (= 42 (eval-expression t-exp env)))
      (is (= 24 (eval-expression f-exp env)))))
  (testing "let-exp"
    (let [env (empty-env)
          exp {:op :let-exp
               :ids ['x 'y]
               :rands [{:op :lit-exp :datum 1}
                       {:op :lit-exp :datum 2}]
               :body {:op :primapp-exp
                      :prim :+
                      :rands [{:op :var-exp
                               :id 'x}
                              {:op :var-exp
                               :id 'y}]}}]
      (is (= 3 (eval-expression exp env)))))
  (testing "proc-exp"
    (let [env (extend-env ['y] [2] (empty-env))
          exp {:op :proc-exp
               :ids ['x]
               :body {:op :primapp-exp
                      :prim :+
                      :rands [{:op :var-exp :id 'x}
                              {:op :var-exp :id 'y}]}}]
      (is (= {:ids ['x]
              :body {:op :primapp-exp
                      :prim :+
                      :rands [{:op :var-exp :id 'x}
                              {:op :var-exp :id 'y}]}
              :env (ExtendedEnv. ['y] [2] (EmptyEnv.))}
             (eval-expression exp env)))))
  (testing "app-exp"
    (let [ae {:op :app-exp
              :rator {:op :var-exp
                      :id 'f}
              :rands [{:op :lit-exp
                       :datum 1}]}
          ev (extend-env ['f] 
                         [{:op :proc-exp
                            :ids ['x]
                            :body {:op :primapp-exp
                                   :prim :+
                                   :rands [{:op :var-exp :id 'x}
                                           {:op :var-exp :id 'y}]}
                            :env (ExtendedEnv. ['y] [2] (EmptyEnv.))}]
                         (empty-env))]
      (is (= 3 (eval-expression ae ev)))))
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
    (is (= 3 (apply-primitive :sub1 [4])))))


(deftest test-apply-procval
  (let [proc {:ids ['x]
              :body {:op :primapp-exp
                     :prim :+
                     :rands [{:op :var-exp :id 'x}
                             {:op :var-exp :id 'y}]}
              :env (ExtendedEnv. '[y] '[2] (EmptyEnv.))}
        args (list 5)]
    (is (= 7 (apply-procval proc args)))))


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
    (is (= true (true-value? 1)))
    (is (= true (true-value? 2)))
    (is (= false (true-value? 0)))))


(deftest test-procval?
  (testing "is procval"
    (is (= true (procval? {:ids ['x 'y]
                           :body {:op :lit-exp :datum 1}
                           :env (EmptyEnv.)}))))
  (testing "is not procval"
    (is (= false (procval? {})))))