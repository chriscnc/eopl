(ns eopl.ribcage-test
  (:require [clojure.test :refer :all]
            [eopl.ribcage :refer :all]))

(deftest all
  (testing "empty-env"
    (is (= '() (empty-env))))
  (testing "extend-env"
    (is (= '(([x y] [4 2])) (extend-env ['x 'y] [4 2] (empty-env)))))
  (testing "apply-env"
    (let [env1 (empty-env)
          env2 (extend-env ['x 'y] [4 2] env1)
          env3 (extend-env ['x 'z] [3 5] env2)]
      (is (thrown? Exception (apply-env env1 'x)))
      (is (= 4 (apply-env env2 'x)))
      (is (= 3 (apply-env env3 'x)))
      (is (= 5 (apply-env env3 'z))))))



