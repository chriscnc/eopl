(ns eopl.ribcage)

(defn empty-env
  []
  (list))


(defn extend-env 
  [syms vals env]
  (conj env (list (into [] syms) (into [] vals))))


(defn apply-env
  [env sym]
  (if (empty? env)
    (throw (Exception. (str "No binding for " sym)))
    (let [syms (ffirst env)
          vals (second (first env))
          env (rest env)]
      (let [pos (.indexOf syms sym)]
        (if (not= pos -1)
          (get vals pos)
          (apply-env env sym))))))


            
