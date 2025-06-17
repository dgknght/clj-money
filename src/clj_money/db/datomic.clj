(ns clj-money.db.datomic
  (:require [clojure.walk :refer [postwalk]]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [datomic.api :as d-peer]
            [datomic.client.api :as d-client]
            [stowaway.datalog :refer [apply-options]]
            [clj-money.db :as db]
            [clj-money.util :as util]
            [clj-money.models :as models]
            [clj-money.db.datomic.tasks :refer [apply-schema]]
            [clj-money.db.datomic.types :refer [coerce-id
                                                ->java-dates]]
            [clj-money.db.datomic.queries :as queries]))

(derive ::db/datomic-peer   ::service)
(derive ::db/datomic-client ::service)

(defprotocol DatomicAPI
  (transact [this tx-data options])
  (query [this arg-map])
  (reset [this]))

(defn- bounding-where-clause
  [crit-or-model-type]
  (let [model-type (util/model-type crit-or-model-type)]
    (case model-type
      :user '[?x :user/email ?user]
      :entity '[?x :entity/name ?entity]
      :commodity '[?x :commodity/symbol ?commodity])))

(def ^:private not-deleted '(not [?x :model/deleted? true]))

(defn- unbounded-query?
  [{:keys [in where]}]
  (and (->> where
            (remove #(or (= not-deleted %)
                         (not= '?x (first %))))
            empty?)
       (not-any? #(= '?x %) in)))

(defn- ensure-bounded-query
  [query criteria]
  (if (unbounded-query? query)
    (assoc-in query [:where] [(bounding-where-clause criteria)])
    query))

(defn- rearrange-query
  "Takes a simple datalog query and adjust the attributes
  to match the format expected by datomic."
  [query]
  (-> query
      (select-keys [:args])
      (assoc :query (dissoc query :args))))

(defn- criteria->query
  [criteria {:as opts :keys [count]}]
  (let [m-type (or (util/model-type criteria)
                   (:model-type opts))]
    (-> {:find (if count
                 '[(count ?x)]
                 '[(pull ?x [*])])
         :in '[$]
         :where [not-deleted]
         :args []}
        (queries/apply-criteria criteria
                                :target m-type
                                :coerce identity)
        (ensure-bounded-query criteria)
        (apply-options (dissoc opts :order-by :sort))
        rearrange-query)))

(defmulti deconstruct util/model-type)
(defmulti before-save util/model-type)
(defmulti after-read util/model-type)
(defmulti prepare-criteria util/model-type)

(defmethod deconstruct :default [m] [m])
(defmethod before-save :default [m] m)
(defmethod after-read :default [m] m)
(defmethod prepare-criteria :default [c] c)

(defmulti ^:private prep-for-put type)

(defmethod prep-for-put ::util/map
  [m]
  (let [[m* nils] (util/split-nils m)]
    (cons (-> m*
              before-save
              ->java-dates
              (util/deep-rename-keys {:id :db/id}))
          (->> nils
               (filter #(-> m meta :clj-money.models/before %))
               (map #(vector :db/retract (:id m) %))))))

#_(def ^:private action-map
  {::db/delete :db/retract
   ::db/put    :db/add})

; Here we expact that the datomic transaction has already been constructed
; like the following:
; [:db/add model-id :user/given-name "John"]
;
; Potentially, it could also look like
; [::db/delete {:id 1 :user/given-name "John"}]
; in which case we want to turn it into
; [:db/retractEntity 1]
(defmethod prep-for-put ::util/vector
  [[_action :as args]]
  ; For now, let's assume a deconstruct fn has prepared a legal datomic transaction
  [args])

(defn- put*
  [models {:keys [api]}]
  {:pre [(sequential? models)]}

  (let [prepped (->> models
                     (map #(util/+id % (comp str random-uuid)))
                     (mapcat deconstruct)
                     (mapcat prep-for-put)
                     vec)
        {:keys [tempids]} (transact api prepped {})]

    (log/debugf "put models %s" prepped)

    ; TODO: relookup the models?
    (->> prepped
         (filter map?)
         (map (comp #(util/deep-rename-keys % {:db/id :id})
                    (fn [m] (update-in m [:db/id] #(tempids % %))))))))

; It seems that after an entire entity has been retracted, the id
; can still be returned
(def ^:private naked-id?
  (every-pred map?
              #(= 1 (count %))
              #(= :db/id (first (keys %)))))

(def ^:private id-entry?
  (every-pred map-entry?
              (comp #(= :id %)
                    first)))

(def ^:private model?
  (every-pred map?
              #(contains? % :id)))

(defn- normalize-criterion
  [x]
  (cond
    (id-entry? x) (update-in x [1] coerce-id)
    (model? x)    (select-keys x [:id])
    :else x))

(defn- normalize-criteria
  [criteria]
  (postwalk normalize-criterion criteria))

(defn- select*
  [criteria {:as options :keys [count]} {:keys [api]}]
  (let [qry (-> criteria
                normalize-criteria
                prepare-criteria
                ->java-dates
                (criteria->query options))
        raw-result (query api qry)]

    (log/debugf "select %s -> %s"
                (models/scrub-sensitive-data criteria)
                qry) ; TODO scrub the datalog query too

    (if count
      (ffirst raw-result)
      (->> raw-result
           (map first)
           (remove naked-id?)
           (map (comp after-read
                      #(util/deep-rename-keys % {:db/id :id})))
           (util/apply-sort options)))))

(defn- delete*
  [models {:keys [api]}]
  {:pre [(and (sequential? models)
              (not-any? nil? models))]}
  (transact api
            (mapv #(vector :db/add (:id %) :model/deleted? true)
                  models)
            {}))

(defmulti init-api ::db/strategy)

(defmethod init-api :clj-money.db/datomic-peer
  [{:keys [uri] :as config}]
  (reify DatomicAPI
    (transact [_ tx-data options]
      @(apply d-peer/transact
              (d-peer/connect uri)
              tx-data
              (mapcat identity options)))
    (query [_ {:keys [query args]}]
      ; TODO: take in the as-of date-time
      (apply d-peer/q
             query
             (cons (-> uri d-peer/connect d-peer/db) args)))
    (reset [_]
      (d-peer/delete-database uri)
      (apply-schema config {:suppress-output? true}))))

(defmethod init-api :clj-money.db/datomic-client
  [{:as config :keys [db-name]}]
  (let [client (d-client/client config)
        conn (d-client/connect client {:db-name db-name})]
    (reify DatomicAPI
      (transact [_ tx-data options]
        (apply d-client/transact
               conn
               {:tx-data tx-data}
               (mapcat identity options)))
      (query [_ {:keys [query args]}]
        ; TODO: take in the as-of date-time
        (apply d-client/q
               query
               (cons (d-client/db conn) args)))
      (reset [_]
        ; probably should not ever get here, as this is for unit tests only
        ))))

(defmethod db/reify-storage ::service
  [config]
  (let [api (init-api config)]
    (reify db/Storage
      (put [_ models]       (put* models {:api api}))
      (select [_ crit opts] (select* crit opts {:api api}))
      (delete [_ models]    (delete* models {:api api}))
      (update [_ _changes _criteria] (throw (UnsupportedOperationException.)))
      (close [_])
      (reset [_]            (reset api)))))
