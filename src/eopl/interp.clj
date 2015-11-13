(ns eopl.interp
  "Simple interpreter AST representation is nested maps.
  :op is a manditory key of every node and is used for 
  dispatch."
  (:require [clojure.pprint :refer [pprint]]
            [eopl.env :refer :all]
            [eopl.ast :refer :all])
  (:import [eopl.ast Closure LitExp VarExp IfExp LetExp ProcExp AppExp 
            PrimExp AddPrim SubPrim MulPrim IncPrim DecPrim]))


(declare eval-rands)
(declare apply-procval)


(defn apply-primitive
  "Evaluate primitive expressions with args that have
  already been evalutated in an environment"
  [prim args]
  (condp = (type prim)
    AddPrim (+ (first args) (second args))
    SubPrim (- (first args) (second args))
    MulPrim (* (first args) (second args))
    IncPrim (inc (first args))
    DecPrim (dec (first args))
    (throw (Exception. (str "Unknown primitive: " prim)))))


(defn true-value?
  "Abstracts our encoding of boolean"
  [x]
  (not (zero? x)))


(defn procval? 
  [x]
  (= Closure (type x)))


(defn eval-expression 
  "Evaluate expressions in an environment. Expression types are...
    :lit-exp - a literal expression
    :var-exp - a variable expression
    :primapp-exp - a primitive operation expression"
  [exp env]
  (condp = (type exp)
    LitExp (:datum exp)
    VarExp (apply-env env (:id exp))
    IfExp (let [{:keys [test-exp true-exp false-exp]} exp]
            (if (true-value? (eval-expression test-exp env))
              (eval-expression true-exp env)
              (eval-expression false-exp env)))
    LetExp (let [ids (:ids exp)
                 rands (eval-rands (:rands exp) env)
                 body (:body exp)]
             (eval-expression body (extend-env ids rands env)))
    ProcExp (Closure. (:ids exp) (:body exp) env)
    AppExp (let [proc (eval-expression (:rator exp) env)
                 args (eval-rands (:rands exp) env)]
             (if (procval? proc)
               (apply-procval proc args)
               (throw (Exception. (str "Attempt to apply non-procedure: " (:rator exp))))))
    PrimExp (apply-primitive (:prim exp)
                             (eval-rands (:rands exp) env))
    (throw (Exception. (str "Unknown expression type: " (:op exp))))))


(defn apply-procval
  "Evaluate the body of a closure in an environment extended
  with args"
  [proc args]
  (let [{:keys [ids body env]} proc]
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
