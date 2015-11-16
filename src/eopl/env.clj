(ns eopl.env
  "The environment is represented by a list of maps"
  (:require [clojure.pprint :refer [pprint]]
            [eopl.ast :refer :all])
  (:import [eopl.ast Closure]))

(defrecord EmptyEnv [])
(defrecord ExtendedEnv [syms vals env])


(defn empty-env 
  "Get an empty environment"
  []
  (EmptyEnv.))


(defn extend-env 
  "Extend an environment with a new set of bindings."
  [syms vals env]
  (ExtendedEnv. (vec syms) (vec vals) env))


(defn extend-env-rec
  "Extend an environment with a new set of bindings recursively."
  [proc-names idss bodies old-env]
  (let [len (count proc-names)
        closures (atom [])
        env (ExtendedEnv. proc-names
                          closures
                          old-env)]
    (do
      (doseq [pos (range len)]
        (swap! closures conj (Closure. (nth idss pos)
                                       (nth bodies pos)
                                       env)))
      env)))


(defn apply-env 
  "Lookup the value of a symbol in the environment."
  [env sym]
  (condp = (type env)
    EmptyEnv (throw (Exception. (format "No binding for '%s'." sym)))
    ExtendedEnv (let [{:keys [syms vals env]} env]
                  (let [pos (.indexOf syms sym)]
                    (if (not= pos -1)
                      (if (instance? clojure.lang.Atom vals)
                        (nth (deref vals) pos)
                        (nth vals pos))
                      (apply-env env sym))))
      (throw (Exception. (format "Invalid env type: %s" env)))))


