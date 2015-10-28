(ns eopl.abs
  "2.2.2 Abstract Syntax and its Representation")

(defrecord VarExp [id])
(defrecord LambdaExp [id body])
(defrecord AppExp [rator rand])

(defn unparse-expr
  [e]
  (condp = (type e)
    VarExp (:id e)
    LambdaExp (list 'lambda 
                    (list (:id e))
                    (unparse-expr (:body e)))
    AppExp (list (unparse-expr (:rator e))
                 (unparse-expr (:rand e)))
    (throw (Exception. "Unmatched type"))))



(defn parse-expr
  [term]
  (cond (symbol? term) (VarExp. term)
        (list? term) (if (= 'lambda (first term))
                       (LambdaExp. (first (second term))
                                   (parse-expr (nth term 2)))
                       (AppExp. (parse-expr (first term))
                                (parse-expr (second term))))
        :else (throw (Exception. (str "Invalid concrete syntax: " term)))))


