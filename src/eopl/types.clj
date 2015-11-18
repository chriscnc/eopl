(ns eopl.types)

; types
(defrecord AtomicType [name])
(defrecord ProcType [arg-types result-type])

; type instances
(def int-type (AtomicType. 'int))
(def bool-type (AtomicType. 'bool))
