(ns eopl.parser-test
  (:require [clojure.test :refer :all]
            [eopl.parser :refer :all]))

(deftest test-parse
  (testing "lit-exp"
    (is (= '{:op :lit-exp 
             :datum 42}
           (parse (read-string "42")))))
  (testing "var-exp"
    (is (= '{:op :var-exp 
             :id x}
           (parse (read-string "x")))))
  (testing "if-exp"
    (is (= '{:op :if-exp
             :test-exp {:op :lit-exp
                        :datum 1}
             :true-exp {:op :lit-exp
                        :datum 2}
             :false-exp {:op :lit-exp
                         :datum 3}}
           (parse (read-string "(if 1 2 3)")))))
  (testing "let-exp"
    (is (= '{:op :let-exp
             :ids [x y]
             :rands [{:op :lit-exp :datum 1}
                     {:op :lit-exp :datum 2}]
             :body {:op :primapp-exp
                    :prim :+
                    :rands [{:op :var-exp
                             :id x}
                            {:op :var-exp
                             :id y}]}}
           (parse (read-string "(let ((x 1) (y 2)) (+ x y))")))))
  (testing "unpack-exp"
    (is (= {:op :unpack-exp
            :ids ['x 'y 'z]
            :exp {:op :primapp-exp
                  :prim :list
                  :rands [{:op :lit-exp :datum 1}
                          {:op :lit-exp :datum 2}
                          {:op :lit-exp :datum 3}]}
            :body {:op :primapp-exp
                   :prim :+
                   :rands [{:op :var-exp :id 'x}
                           {:op :primapp-exp
                            :prim :+
                            :rands [{:op :var-exp :id 'y}
                                    {:op :var-exp :id 'z}]}]}}
           (parse (read-string "(unpack (x y z) (list 1 2 3) (+ x (+ y z)))")))))
  (testing "primapp-exp +"
    (is (= '{:op :primapp-exp 
             :prim :+
             :rands [{:op :lit-exp
                      :datum 24}
                     {:op :lit-exp
                      :datum 24}]}
           (parse (read-string "(+ 24 24)")))))
  (testing "primapp-exp -"
    (is (= '{:op :primapp-exp 
             :prim :-
             :rands [{:op :lit-exp
                      :datum 48}
                     {:op :lit-exp
                      :datum 6}]}
           (parse (read-string "(- 48 6)")))))
  (testing "primapp-exp *"
    (is (= '{:op :primapp-exp 
             :prim :*
             :rands [{:op :lit-exp
                      :datum 6}
                     {:op :lit-exp
                      :datum 7}]}
           (parse (read-string "(* 6 7)")))))
  (testing "primapp-exp add1"
    (is (= '{:op :primapp-exp 
             :prim :add1
             :rands [{:op :lit-exp
                      :datum 41}]}
           (parse (read-string "(add1 41)")))))
  (testing "primapp-exp *"
    (is (= '{:op :primapp-exp 
             :prim :sub1
             :rands [{:op :lit-exp
                      :datum 43}]}
           (parse (read-string "(sub1 43)")))))

  (testing "primapp-exp list"
    (is (= '{:op :primapp-exp 
             :prim :list
             :rands [{:op :lit-exp
                      :datum 4}
                     {:op :lit-exp
                      :datum 2}]}
           (parse (read-string "(list 4 2)")))))

  (testing "primapp-exp car"
    (is (= {:op :primapp-exp 
             :prim :car
             :rands [{:op :primapp-exp
                      :prim :list
                      :rands [{:op :lit-exp
                               :datum 1}
                              {:op :lit-exp
                               :datum 2}
                              {:op :lit-exp
                               :datum 3}]}]}
           (parse (read-string "(car (list 1 2 3))")))))

  (testing "primapp-exp cdr"
    (is (= {:op :primapp-exp 
             :prim :cdr
             :rands [{:op :primapp-exp
                      :prim :list
                      :rands [{:op :lit-exp
                               :datum 1}
                              {:op :lit-exp
                               :datum 2}
                              {:op :lit-exp
                               :datum 3}]}]}
           (parse (read-string "(cdr (list 1 2 3))")))))

  (testing "primapp-exp cons"
    (is (= {:op :primapp-exp 
             :prim :cons
             :rands [{:op :lit-exp
                      :datum 4}
                     {:op :primapp-exp
                      :prim :list
                      :rands [{:op :lit-exp
                               :datum 1}
                              {:op :lit-exp
                               :datum 2}
                              {:op :lit-exp
                               :datum 3}]}]}
           (parse (read-string "(cons 4 (list 1 2 3))")))))
  )


