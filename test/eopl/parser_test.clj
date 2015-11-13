(ns eopl.parser-test
  (:require [clojure.test :refer :all]
            [eopl.parser :refer :all]
            [eopl.ast :refer :all])
  (:import [eopl.ast LitExp VarExp IfExp LetExp ProcExp AppExp 
            PrimExp AddPrim SubPrim MulPrim IncPrim DecPrim]))

(deftest test-parse
  (testing "lit-exp"
    (is (= (LitExp. 42)
           (parse (read-string "42")))))
  (testing "var-exp"
    (is (= (VarExp. 'x)
           (parse (read-string "x")))))
  (testing "if-exp"
    (is (= (IfExp. (LitExp. 1)
                   (LitExp. 2)
                   (LitExp. 3))
           (parse (read-string "(if 1 2 3)")))))
  (testing "let-exp"
    (is (= (LetExp. ['x 'y]
                    [(LitExp. 1) (LitExp. 2)]
                    (PrimExp. (AddPrim.)
                              [(VarExp. 'x) (VarExp. 'y)]))
           (parse (read-string "(let ((x 1) (y 2)) (+ x y))")))))
  (testing "proc-exp"
    (is (= (ProcExp. ['x 'y]
                     (PrimExp. (AddPrim.)
                               [(VarExp. 'x) (VarExp. 'y)]))
           (parse (read-string "(proc (x y) (+ x y))")))))
  (testing "app-exp"
    (is (= (AppExp. (VarExp. 'f)
                    [(LitExp. 1) (LitExp. 2)])
           (parse (read-string "(f 1 2)")))))
  (testing "prim-exp +"
    (is (= (PrimExp. (AddPrim.)
                     [(LitExp. 24) (LitExp. 24)])
           (parse (read-string "(+ 24 24)"))))
    (is (thrown? Exception (parse (read-string "(+ 1 2 3)")))))
  (testing "prim-exp -"
    (is (= (PrimExp. (SubPrim.)
                     [(LitExp. 48) (LitExp. 6)])
           (parse (read-string "(- 48 6)"))))
    (is (thrown? Exception (parse (read-string "(- 1 2 3)")))))
  (testing "prim-exp *"
    (is (= (PrimExp. (MulPrim.)
                     [(LitExp. 6) (LitExp. 7)])
           (parse (read-string "(* 6 7)"))))
    (is (thrown? Exception (parse (read-string "(* 1 2 3)")))))
  (testing "prim-exp add1"
    (is (= (PrimExp. (IncPrim.)
                     [(LitExp. 41)])
           (parse (read-string "(add1 41)"))))
    (is (thrown? Exception (parse (read-string "(add1 1 2)")))))
  (testing "prim-exp sub1"
    (is (= (PrimExp. (DecPrim.)
                     [(LitExp. 43)])
           (parse (read-string "(sub1 43)"))))
    (is (thrown? Exception (parse (read-string "(sub1 1 2)")))))
  )


