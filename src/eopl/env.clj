(ns eopl.env
  "The environment is represented by a list of maps"
  (:require [clojure.pprint :refer [pprint]]))

(defrecord EmptyEnv [])
(defrecord ExtendedEnv [syms vals env])
(defrecord RecExtendedEnv [proc-names idss bodies env])


(defn empty-env 
  "Get an empty environment"
  []
  (EmptyEnv.))


(defn extend-env 
  "Extend and environment with a new set of bindings.
  'bindings is a map."
  [syms vals env]
  (ExtendedEnv. (vec syms) (vec vals) env))


(defn apply-env 
  "Lookup the value of a symbol in the environment."
  [env sym]
  (condp = (type env)
    EmptyEnv (throw (Exception. (format "No binding for '%s'." sym)))
    ExtendedEnv (let [{:keys [syms vals env]} env]
                  (let [pos (.indexOf syms sym)]
                    (if (not= pos -1)
                      (nth vals pos)
                      (apply-env env sym))))
      (throw (Exception. (format "Invalid env type: %s" env)))))


