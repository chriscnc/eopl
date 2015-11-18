(ns eopl.core-test
  (:require [clojure.test :refer :all]
            [eopl.core :refer :all]))


(deftest test-interpreter
  (is (= 1 (run "1")))
  (is (= 3 (run "(+ 1 2)")))
  (is (= 0 (run "(- 2 2)")))
  (is (= 6 (run "(* 3 2)")))
  (is (= 2 (run "(add1 1)")))
  (is (= 1 (run "(sub1 2)")))
  (is (= true (run "(zero? 0)")))
  (is (= false (run "(zero? 1)")))
  (is (= 1 (run "(if (zero? 0) 1 2)")))
  (is (= 4 (run "(if (- 2 1) 
                   (add1 3) 
                   (sub1 3))")))
  (is (= 3 (run "(let ((x 1) (y 2)) 
                   (+ x y))")))
  (is (= 3 (run "(let ((x 1) (y 2)) 
                   (+ x y))")))
  (is (= 3 (run "(let ((x 5))
                   (let ((x 1) (y 2))
                     (+ x y)))")))
  (is (= 3 (run "(let ((x 1)) 
                   (let ((y 2)) 
                     (+ x y)))")))
  (is (= 8 (run "(let ((x 5))
                   (let ((f (proc (y) (+ x y))))
                     (f 3)))")))
  (is (= 176 (run "(let ((x 5))
                     (let ((x 38)
                           (f (proc (y z) (* y (+ x z))))
                           (g (proc (u) (+ u x))))
                       (f (g 3) 17)))")))
  (is (= 120 (run "(letrec ((fact (proc (n)
                                    (if (zero? n)
                                      1
                                      (* n (fact (sub1 n)))))))
                    (fact 5))")))
  (is (= 1 (run "(letrec ((even (proc (x) (if (zero? x) 1 (odd (sub1 x)))))
                          (odd (proc (x) (if (zero? x) 0 (even (sub1 x))))))
                  (odd 13))")))
                     

)

       
