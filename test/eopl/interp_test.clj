(ns eopl.interp-test
  (:require [clojure.test :refer :all]
            [eopl.interp :refer :all]))

(deftest test-true-value?
  (testing "true-value?"
    (is (= true (true-value? 1)))
    (is (= true (true-value? 2)))
    (is (= false (true-value? 0)))))


