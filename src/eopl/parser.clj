(ns eopl.parser
  "We will encode the program text as a Lisp to avoid writing a scanner as
  this is not the focus of this project"
  (:refer-clojure :exclude [print]))

(defn parse
  "parse is passed a element 'elt' as generated by read-string on the program text"
  [elt]
  (cond (number? elt) {:op :lit-exp
                       :datum elt}
        (symbol? elt) {:op :var-exp
                       :id elt}
        (list? elt) (let [rator (first elt)
                          rands (rest elt)]
                      (cond (= rator '+) {:op :primapp-exp
                                          :prim :+
                                          :rands (map parse rands)}
                            (= rator '-) {:op :primapp-exp
                                          :prim :-
                                          :rands (map parse rands)}
                            (= rator '*) {:op :primapp-exp
                                          :prim :*
                                          :rands (map parse rands)}
                            (= rator 'add1) {:op :primapp-exp
                                          :prim :add1
                                          :rands (map parse rands)}
                            (= rator 'sub1) {:op :primapp-exp
                                          :prim :sub1
                                          :rands (map parse rands)}
                            (= rator 'print) {:op :primapp-exp
                                          :prim :print
                                          :rands (map parse rands)}
                            :else (throw (Exception. (str "Unknown rator: " rator)))))
        :else (throw (Exception. (str "Invalid element: " elt)))))
    



