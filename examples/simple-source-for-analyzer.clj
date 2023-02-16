(ns core)

(s/defschema SchemaA
  {:a s/Int
   :b s/Str})

(s/def A :- SchemaA
  {:a 1
   :b 2}) ;; error

(s/defn add :- s/Str
  [a :- s/Int
   b :- s/Int]
  (+ a b))

(defn main []
  (s/let [a :- s/Str 1] ;; error
    (add 1 a))) ;; error
