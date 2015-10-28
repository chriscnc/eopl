(ns eopl.abs-test
  (:require [clojure.test :refer :all]
            [eopl.abs :refer :all]))


(deftest parse-unparse
  (let [exp '((lambda (a b c) 
                      (if (eqv? b c) (lambda (c) (cons a c))
                        a))
              1 2 3)]
    (is (= exp (unparse-ast (parse-expr exp))))))


(deftest test-get-lex-dist
  (let [positions '({a 0} {a 1 b 2})]
    (is (= {:pos 0 :depth 0} (get-lex-dist 'a positions)))
    (is (= {:pos 2 :depth 1} (get-lex-dist 'b positions)))
    (is (nil? (get-lex-dist 'c positions)))))


(deftest test-lex-addr
  (let [exp '((lambda (a b c) 
                      (if (eqv? b c) 
                        (lambda (c) (cons a c))
                        a))
              1 2 3)
        lex-exp '((lambda (a b c) 
                       (if ((eqv? free) (b 0 1) (c 0 2)) 
                         (lambda (c) ((cons free) (a 1 0) (c 0 0)))
                         (a 0 0)))
               1 2 3)]
    (is (= lex-exp (unparse-lex-ast (lexical-address (parse-expr exp)))))))
  
