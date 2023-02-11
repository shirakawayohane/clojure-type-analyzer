(ns test.name.space
  (:require
   [test.name.space.handlers]
   [test.name.space.util :as u]
   [test.name.space.entity.sample :as e.sample]
   [test.name.space.core.sample :as core]
   [struct.core :as v]))

(def SampleSchema
  {:id [v/required v/uuid]
   :name [v/required v/uuid]
   :value [v/required v/string v/max-count-512]})

(def add-sample-schema
  SampleSchema)

(defmethod h/handler [::r/sample :post]
  [{:as req :keys [db params]}]
  (let [[errors {:as values :keys [id value name]}] (v/validate params add-sample-schema)]
    (if errors
      (res/bad-request errors)
      (if (e.sample/find-by-id db id)
        (res/conflict)
        (if-let [result (core/add-sample req values)]
          (res/ok result)
          (res/internal-server-error))))))
