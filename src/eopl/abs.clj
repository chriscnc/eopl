(ns eopl.abs
  "Exercise-2.7")

(defrecord LitExp [datum])
(defrecord VarExp [id])
(defrecord IfExp [test-exp true-exp false-exp])
(defrecord LambdaExp [ids body])
(defrecord AppExp [rator rands])
(defrecord LexInfo [id depth position])
(defrecord FreeInfo [id])

(defn unparse-ast
  [node]
  (condp = (type node)
    LitExp (:datum node)
    VarExp (:id node)
    IfExp (list 'if
                (unparse-ast (:test-exp node))
                (unparse-ast (:true-exp node))
                (unparse-ast (:false-exp node)))
    LambdaExp (list 'lambda 
                    (:ids node)
                    (unparse-ast (:body node)))
    AppExp `(~(unparse-ast (:rator node))
              ~@(map unparse-ast (:rands node)))
    (throw (Exception. "Unmatched type"))))


(defn parse-expr
  [term]
  (cond (number? term) (LitExp. term)
        (symbol? term) (VarExp. term)
        (list? term) 
        (condp = (first term)
          'if (IfExp. (parse-expr (second term))
                      (parse-expr (nth term 2))
                      (parse-expr (nth term 3)))
          'lambda (LambdaExp. (second term)
                              (parse-expr (nth term 2)))
          (AppExp. (parse-expr (first term))
                   (map parse-expr (rest term))))
        :else (throw (Exception. (str "Invalid concrete syntax: " term)))))


(defn get-lex-dist
  "Retrieves the position of the first var in a list of var/positions
  that represents a lexical environment."
  [id lex-env]
  (let [f (fn [id lex-env depth]
            (when (not (empty? lex-env))
              (let [bindings (first lex-env)]
                (if (contains? bindings id)
                  {:pos (get bindings id)
                   :depth depth}
                  (recur id (rest lex-env) (inc depth))))))
        init-depth 0]
    (f id lex-env init-depth)))



(defn lex-addr
  "Compute the lexical distance for all vars in an ast and emit
  a new ast with VarExp nodes replaced with either LexInfo or FreeInfo
  nodes."
  [node lex-env]
  (condp = (type node)
    LitExp node
    VarExp (let [id (:id node)
                 dist (get-lex-dist id lex-env)]
             (if dist
               (LexInfo. id (:depth dist) (:pos dist))
               (FreeInfo. id)))
    IfExp (IfExp. (lex-addr (:test-exp node) lex-env)
                  (lex-addr (:true-exp node) lex-env)
                  (lex-addr (:false-exp node) lex-env))
    LambdaExp (let [ids (:ids node)
                    lex-env (cons (zipmap ids (range)) lex-env)]
                (LambdaExp. ids
                            (lex-addr (:body node) 
                                      lex-env)))
    AppExp (AppExp. (lex-addr (:rator node) lex-env)
                    (map #(lex-addr % lex-env) (:rands node)))
    (throw (Exception. "Unmatched type"))))


(defn lexical-address
  "Wrapper for the multi-arg lexical-address"
  [ast]
  (lex-addr ast '()))


(defn unparse-lex-ast
  [node]
  (condp = (type node)
    LitExp (:datum node)
    FreeInfo (list (:id node) 'free)
    LexInfo (list (:id node) (:depth node) (:position node))
    IfExp (list 'if
                (unparse-lex-ast (:test-exp node))
                (unparse-lex-ast (:true-exp node))
                (unparse-lex-ast (:false-exp node)))
    LambdaExp (list 'lambda 
                    (:ids node)
                    (unparse-lex-ast (:body node)))
    AppExp `(~(unparse-lex-ast (:rator node))
              ~@(map unparse-lex-ast (:rands node)))
    (throw (Exception. (str "Unmatched type" node)))))

