(ns eopl.interp
  "Simple interpreter AST representation is nested maps.
  :op is a manditory key of every node and is used for 
  dispatch."
  (:require [eopl.env :refer :all]))


(defmulti eval-expression
  "Dispatch on the :op key of 'exp'. Possible :op values
  are...

  :lit-exp - A literal expression, return the :datum.

  :var-exp - A variable expression, return the value of 
             :id in the environment
  "
  {:arglists '([exp env])}
  (fn [exp env] (:op exp)))


(defmethod eval-expression :lit-exp
  [exp env]
  (:datum exp))
  

(defmethod eval-expression :var-exp
  [exp env]
  (apply-env env (:id exp)))

