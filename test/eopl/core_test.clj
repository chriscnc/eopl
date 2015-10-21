(ns eopl.core-test
  (:require [clojure.test :refer :all]
            [eopl.env :refer :all]
            [eopl.interp :refer :all]
            [eopl.parser :refer :all]))


(deftest test-comparison-operators
  (let [env (empty-env)
        env-x (extend-env env {'x 5})
        eval-str (fn [exp-str env] 
                   (eval-expression (parse (read-string exp-str)) env))]
    (testing "program fragments"
      (are [expected actual] (= expected actual)
           1 (eval-str "1" env)
           5 (eval-str "x" env-x)
           3 (eval-str "(+ 1 2)" env)
           0 (eval-str "(- 2 2)" env)
           6 (eval-str "(* 3 2)" env)
           2 (eval-str "(add1 1)" env)
           1 (eval-str "(sub1 2)" env)
           (list 1 2 3) (eval-str "(list 1 2 3)" env)
           1 (eval-str "(car (list 1 2 3))" env)
           (list 2 3) (eval-str "(cdr (list 1 2 3))" env)
           (list 1 2 3) (eval-str "(cons 1 (list 2 3))" env)
           2 (eval-str "(if (- 2 2) (add1 3) (sub1 3))" env)
           4 (eval-str "(if (- 2 1) (add1 3) (sub1 3))" env)
           3 (eval-str "(let ((x 1) (y 2)) (+ x y))" env)
           7 (eval-str "(let ((y 2)) (+ x y))" env-x)
           3 (eval-str "(let ((x 1) (y 2)) (+ x y))" env-x)
           1 (eval-str "(let ((x 1)) (let ((y x)) y))" env)
           6 (eval-str "(unpack (x y z) (list 1 2 3) (+ x (+ y z)))" env)))
    (testing "unpack wrong expression type"
      (is (thrown? Exception (eval-str "(unpack (x y z) 6 (+ x y))" env))))
    (testing "unpack wrong number of values to unpack"
      (is (thrown? Exception (eval-str "(unpack (x y z) (list 1 2) (+ x y)))" env))))
    ))



