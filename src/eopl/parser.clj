(ns eopl.parser
  "We will encode the program text as a Lisp to avoid writing a scanner as
  this is not the focus of this project"
  (:require [eopl.ast :refer :all]
            [eopl.types :refer :all])
  (:import [eopl.ast LitExp TrueExp FalseExp VarExp IfExp LetExp LetRecExp ProcExp AppExp 
            PrimExp AddPrim SubPrim MulPrim IncPrim DecPrim ZeroPrim]))

(declare parse-type)

(defn parse
  "parse is passed a element 'elt' as generated by read-string on the program text"
  [elt]
  (cond (number? elt) (LitExp. elt)
        (symbol? elt) (VarExp. elt)
        (true? elt) (TrueExp.)
        (false? elt) (FalseExp.)
        (list? elt) 
        (let [rator (first elt)
              rands (rest elt)
              rand-count (count rands)]
          (condp = rator 
            '+ (if (= 2 rand-count)
                 (PrimExp. (AddPrim.) 
                           (map parse rands))
                 (throw (Exception. (format "+ takes 2 arguments, given %d." rand-count))))
            '- (if (= 2 rand-count)
                 (PrimExp. (SubPrim.)
                           (map parse rands))
                 (throw (Exception. (format "- takes 2 arguments, given %d." rand-count))))
            '* (if (= 2 rand-count)
                 (PrimExp. (MulPrim.)
                           (map parse rands))
                 (throw (Exception. (format "* takes 2 arguments, given %d." rand-count))))
            'add1 (if (= 1 rand-count)
                    (PrimExp. (IncPrim.)
                              (map parse rands))
                    (throw (Exception. (format "add1 takes 1 arguments, given %d." rand-count))))
            'sub1 (if (= 1 rand-count)
                    (PrimExp. (DecPrim.)
                              (map parse rands))
                    (throw (Exception. (format "sub1 takes 1 arguments, given %d." rand-count))))
            'zero? (if (= 1 rand-count)
                     (PrimExp. (ZeroPrim.)
                               (map parse rands)))
            'if (IfExp. (parse (nth rands 0))
                        (parse (nth rands 1))
                        (parse (nth rands 2)))
            'let (let [bindings (first rands)
                       body (parse (second rands))
                       ids (map first bindings)
                       rands (map #(parse (second %)) bindings)]
                   (LetExp. ids rands body))
            'letrec (let [bindings (first rands)
                          proc-names (map first bindings)
                          procs (map parse (map second bindings))
                          idss (map :ids procs)
                          bodies (map :body procs)
                          let-rec-body (parse (second rands))]
                      (LetRecExp. proc-names idss bodies let-rec-body))
            'proc (ProcExp. (map parse-type (take-nth 2 (rest (first rands))))
                            (take-nth 2 (first rands))
                            (parse (second rands)))
            ; else it's an app-exp
            (AppExp. (VarExp. rator)
                     (map parse rands))))
        :else (throw (Exception. (str "Invalid element: " elt)))))


(defn parse-type
  [texp]
  (condp = texp
    'int int-type
    'bool bool-type
    (throw (Exception. (str "Unknown type expression: " texp)))))


