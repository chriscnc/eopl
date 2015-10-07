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

  )


