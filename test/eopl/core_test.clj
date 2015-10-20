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
           2 (eval-str "(if (- 2 2) (add1 3) (sub1 3))" env)
           4 (eval-str "(if (- 2 1) (add1 3) (sub1 3))" env)
           3 (eval-str "(let ((x 1) (y 2)) (+ x y))" env)
           7 (eval-str "(let ((y 2)) (+ x y))" env-x)
           3 (eval-str "(let ((x 1) (y 2)) (+ x y))" env-x)
           1 (eval-str "(let ((x 1)) (let ((y x)) y))" env)
           ))))

