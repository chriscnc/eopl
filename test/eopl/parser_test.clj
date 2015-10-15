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
  (testing "primapp-exp equal?"
    (is (= '{:op :bool-exp
             :prim :equal?
             :rands [{:op :lit-exp
                      :datum 4}
                     {:op :lit-exp
                      :datum 4}]}
           (parse (read-string "(equal? 4 4)")))))
  (testing "primapp-exp zero?"
    (is (= '{:op :bool-exp
             :prim :zero?
             :rands [{:op :lit-exp
                      :datum 4}]}
           (parse (read-string "(zero? 4)")))))
  (testing "primapp-exp greater?"
    (is (= '{:op :bool-exp
             :prim :greater?
             :rands [{:op :lit-exp
                      :datum 4}
                     {:op :lit-exp
                      :datum 4}]}
           (parse (read-string "(greater? 4 4)")))))
  (testing "primapp-exp less?"
    (is (= '{:op :bool-exp
             :prim :less?
             :rands [{:op :lit-exp
                      :datum 4}
                     {:op :lit-exp
                      :datum 4}]}
           (parse (read-string "(less? 4 4)")))))
  (testing "primapp-exp add1"
    (is (= '{:op :primapp-exp 
             :prim :add1
             :rands [{:op :lit-exp
                      :datum 41}]}
           (parse (read-string "(add1 41)")))))
  (testing "primapp-exp sub1"
    (is (= '{:op :primapp-exp 
             :prim :sub1
             :rands [{:op :lit-exp
                      :datum 43}]}
           (parse (read-string "(sub1 43)")))))

  )


