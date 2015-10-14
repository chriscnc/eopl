(ns eopl.util-test
  (:require [clojure.test :refer :all]
            [eopl.util :refer :all]))


(deftest test-bool?
  (testing "bool?"
    (is (bool? true))
    (is (bool? false))
    (is (not (bool? 1)))
    (is (not (bool? 0)))))
