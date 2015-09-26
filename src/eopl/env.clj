(ns eopl.env
  "The environment is represented by a list of maps")

(defn empty-env 
  "Get an empty environment"
  []
  '())


(defn extend-env 
  "Extend and environment with a new set of bindings."
  [env bindings]
  (conj env bindings))


(defn apply-env [env sym]
  "Lookup the value of a symbol in the environment."
  (if (empty? env)
    (throw (Exception. (format "No binding for '%s'." sym)))
    (let [bindings (first env)
          encl-env (rest env)]
      (if (contains? bindings sym)
        (get bindings sym)
        (recur encl-env sym)))))
