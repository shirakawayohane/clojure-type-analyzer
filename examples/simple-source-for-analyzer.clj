(ns core)

(s/defn add :- s/Str
  [a :- s/Int
   b :- s/Int]
  (+ a b))

(defn main []
  (s/let [a :- s/Str 1]
    (add 1 a)))
