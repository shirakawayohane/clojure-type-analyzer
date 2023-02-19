(ns core)

(s/defn add-one :- s/Str ;; サンプルコード
  [a :- s/Int]
  (+ a 1))

(def anonymous-fn #(* %1 %2 %3))

(s/defschema SchemaA
  {:a s/Int
   :b s/Str})

(s/def A :- SchemaA
  {:a 1
   :b 2}) ;; error

(s/defn add :- s/Str
  [a :- s/Int
   b :- s/Str]
  (+ a b)) ;; error

(defn main []
  (s/let [a :- s/Str 1] ;; error
    (add 1 a))) ;; error
