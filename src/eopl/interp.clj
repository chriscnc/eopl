(ns eopl.interp
  "Simple interpreter AST representation is nested maps.
  :op is a manditory key of every node and is used for 
  dispatch."
  (:require [eopl.env :refer :all]
            [eopl.util :as util]))


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
    :equal? (= (first args) (second args))
    :zero? (= (first args) 0)
    :greater? (> (first args) (second args)) 
    :less? (< (first args) (second args))
    (throw (Exception. (str "Unknown primitive: " prim)))))


(declare eval-rands)


(defn true-value?
  "Abstracts our encoding of boolean."
  [x]
  (true? x))


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
                  false-exp (:false-exp exp)
                  test-val (eval-expression test-exp env)]
              (if (util/bool? test-val)
                (if (true-value? (eval-expression test-exp env))
                  (eval-expression true-exp env)
                  (eval-expression false-exp env))
                (throw (Exception. "Non-boolean value in test-exp of 'if'"))))
    :primapp-exp (apply-primitive (:prim exp)
                                  (eval-rands (:rands exp) env))
    (throw (Exception. (str "Unknown expression type: " (:op exp))))))


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
