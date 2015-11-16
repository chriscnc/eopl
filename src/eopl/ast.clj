(ns eopl.ast)

; Expressions
(defrecord LitExp [datum])
(defrecord VarExp [id])
(defrecord IfExp [test-exp true-exp false-exp])
(defrecord LetExp [ids rands body])
(defrecord LetRecExp [proc-names idss bodies letrec-body])
(defrecord ProcExp [ids body])
(defrecord AppExp [rator rands])
(defrecord PrimExp [prim rands])

; Primatives
(defrecord AddPrim [])
(defrecord SubPrim [])
(defrecord MulPrim [])
(defrecord IncPrim [])
(defrecord DecPrim [])
(defrecord ZeroPrim [])

; Procval
(defrecord Closure [ids body env])
