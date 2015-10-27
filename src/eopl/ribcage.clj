(ns eopl.ribcage)

(defn empty-env
  []
  (list))


(defn extend-env 
  [syms vals env]
  (conj env (list (into [] vals))))


(defn apply-env
  [env depth position]
  (if (empty? env)
    (throw (Exception. (str "No binding at: d=" depth ",p=" position)))
    (if (zero? depth)
      (get (ffirst env) position)
      (apply-env (rest env) (dec depth) position))))



            
