(ns eopl.interp
  "Simple interpreter AST representation is nested maps.
  :op is a manditory key of every node and is used for 
  dispatch."
  (:require [clojure.pprint :refer [pprint]]
            [eopl.env :refer :all]))

(declare eval-rands)
(declare apply-procval)

(defn apply-primitive
  "Evaluate primitive expressions with args that have
  already been evalutated in an environment"
  [prim args]
  (case prim
    :+ (+ (first args) (second args))
    :- (- (first args) (second args))
    :* (* (first args) (second args))
    :add1 (inc (first args))
    :sub1 (dec (first args))
    (throw (Exception. (str "Unknown primitive: " prim)))))


(defn true-value?
  "Abstracts our encoding of boolean"
  [x]
  (not (zero? x)))


(defn procval? 
  [x]
  (and (contains? x :ids)
       (contains? x :body)
       (contains? x :env)))


(defn make-closure
  "Construct a closure"
  [ids body env]
  {:ids ids
   :body body
   :env env})


(defn eval-expression 
  "Evaluate expressions in an environment. Expression types are...
    :lit-exp - a literal expression
    :var-exp - a variable expression
    :primapp-exp - a primitive operation expression"
  [exp env]
  (case (:op exp)
    :lit-exp (:datum exp)
    :var-exp (apply-env env (:id exp))
    :if-exp (let [test-exp (:test-exp exp)
                  true-exp (:true-exp exp)
                  false-exp (:false-exp exp)]
              (if (true-value? (eval-expression test-exp env))
                (eval-expression true-exp env)
                (eval-expression false-exp env)))
    :let-exp (let [ids (:ids exp)
                   rands (eval-rands (:rands exp) env)
                   body (:body exp)]
               (eval-expression body (extend-env ids rands env)))
    :proc-exp (make-closure (:ids exp) (:body exp) env)
    :app-exp (let [proc (eval-expression (:rator exp) env)
                   args (eval-rands (:rands exp) env)]
               (if (procval? proc)
                 (apply-procval proc args)
                 (throw (Exception. (str "Attempt to apply non-procedure: " (:rator exp))))))
    :primapp-exp (apply-primitive (:prim exp)
                                  (eval-rands (:rands exp) env))
    (throw (Exception. (str "Unknown expression type: " (:op exp))))))


(defn apply-procval
  "Evaluate the body of a closure in an environment extended
  with args"
  [proc args]
  (let [ids (:ids proc)
        body (:body proc)
        env (:env proc)]
    (eval-expression body (extend-env ids args env))))


(defn eval-rands
  "Evaluate a list of operands in an environment"
  [rands env]
  (map #(eval-expression % env) rands))


;; our language doesn't have statements, so we don't need this yet.
(defn eval-program
  "Evaluates a program"
  [pgm]
  (case (:op pgm)
    :a-program (eval-expression (:exp pgm) (empty-env))
    (throw (Exception. (str "Unknown op code: " (:op pgm))))))
