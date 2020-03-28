(ns clj-money.models)

(defn tag
  ([model]
   {:pre [model]}
   (or (-> model meta ::model)
       (throw (ex-info "Unable to determine the model type." {:model model}))))
  ([model tag]
   (vary-meta model
              assoc
              ::model
              (keyword "clj-money.models" (name tag)))))
