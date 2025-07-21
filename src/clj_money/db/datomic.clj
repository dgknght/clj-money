(ns clj-money.db.datomic
  (:require [clojure.walk :refer [postwalk]]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [datomic.api :as d-peer]
            [datomic.client.api :as d-client]
            [stowaway.datalog :as dtl]
            [clj-money.config :refer [env]]
            [clj-money.db :as db]
            [clj-money.util :as util]
            [clj-money.models :as models]
            [clj-money.models.schema :as schema]
            [clj-money.db.datomic.tasks :refer [apply-schema]]
            [clj-money.db.datomic.types :refer [coerce-id
                                                apply-coercions
                                                ->java-dates]]
            [clj-money.db.datomic.queries :as queries]))

(derive ::db/datomic-peer   ::service)
(derive ::db/datomic-client ::service)

(defprotocol DatomicAPI
  (transact [this tx-data options])
  (query [this arg-map])
  (reset [this]))

; TODO: Get this from the schema
(defn- bounding-where-clause
  [model-type]
  {:pre [model-type]}
  (case model-type
    :budget           '[?x :budget/start-date ?budget-start-date]
    :budget-item      '[?x :budget-item/periods ?budget-item-periods]
    :user             '[?x :user/email ?user-email]
    :entity           '[?x :entity/name ?entity-name]
    :grant            '[?x :grant/user ?grant-user]
    :cached-price     '[?x :cached-price/trade-date ?cached-price-trade-date]
    :commodity        '[?x :commodity/symbol ?commodity-symbol]
    :price            '[?x :price/value ?price-value]
    :account          '[?x :account/type ?account-type]
    :attachment       '[?x :attachment/caption ?attachment-caption]
    :transaction      '[?x :transaction/description ?transaction-description]
    :transaction-item '[?x :transaction-item/action ?transaction-item-action]
    :image            '[?x :image/uuid ?image-uuid]
    :reconciliation   '[?x :reconciliation/status ?reconciliation-status]
    :lot              '[?x :lot/purchase-date ?lot-purchase-date]))

(def ^:private not-deleted '(not [?x :model/deleted? true]))

(defn- rearrange-query
  "Takes a simple datalog query and adjust the attributes
  to match the format expected by datomic."
  [query]
  (-> query
      (select-keys [:args])
      (assoc :query (dissoc query :args))))

(def ^:private recursion-keys
  {:account :account/parent})

(defn- recursion
  [{:keys [include-children? include-parents?]} model-type]
  (when (or include-children? include-parents?)
    (if-let [k (recursion-keys model-type)]
      (cond-> [k]
        include-parents? (conj :upward))
      (throw (IllegalArgumentException. (format "No recursion defined for model type %s" model-type))))))

(defn- criteria->query
  [criteria {:as opts :keys [count]}]
  (let [m-type (or (util/model-type criteria)
                   (:model-type opts))]
    (-> {:find (if count
                 '[(count ?x)]
                 '[(pull ?x [*])])
         :in '[$]
         :where [not-deleted
                 (bounding-where-clause m-type)]
         :args []}
        (queries/apply-criteria criteria
                                :target m-type
                                :coerce identity
                                :recursion (recursion opts m-type))
        (dtl/apply-options (dissoc opts :order-by :sort))
        (queries/apply-select opts)
        rearrange-query)))

(defmulti deconstruct util/model-type)
(defmulti before-save util/model-type)
(defmulti after-read util/model-type)
(defmulti prepare-criteria util/model-type)
(defmulti propagate-delete util/model-type-dispatch)

(defmethod deconstruct :default [m] [m])
(defmethod before-save :default [m] m)
(defmethod after-read :default [m] m)
(defmethod prepare-criteria :default [c] c)
(defmethod propagate-delete :default [m _] [m])

(defmulti ^:private prep-for-put type)

(defmethod prep-for-put ::util/map
  [m]
  (let [m* (util/remove-nils m)
        nils (util/locate-nils m)]
    (cons (-> m*
              before-save
              ->java-dates
              (util/deep-rename-keys {:id :db/id}))
          (->> nils
               (filter #(get-in (-> m meta :clj-money.models/before) %))
               (map #(vector :db/retract
                             (get-in m (concat
                                         (butlast %)
                                         [:id]))
                             (last %)))))))

; Here we expact that the datomic transaction has already been constructed
; like the following:
; [:db/add model-id :user/given-name "John"]
;
; Potentially, it could also look like
; [::db/delete {:id 1 :user/given-name "John"}]
; in which case we want to turn it into
; [:db/retract 1]
(defmethod prep-for-put ::util/vector
  [[action & [{:keys [id] :as model} :as args]]]
  (if (= ::db/delete action)
    (->> (keys model)
         (remove #(= :id %))
         (map #(vector :db/retract id %)))
    [(apply vector :db/add args)]))

(defn- models->refs
  [m]
  (postwalk (fn [x]
              (if (and (map-entry? x)
                       (schema/model-ref-keys (key x)))
                (update-in x [1] :id)
                x))
            m))

(defn- pass-through
  "Returns a fn that takes a single argument and if that
  argument is a vector, returns it unchanged (or wrapped in another
  vector if :plural is true). Otherwise it applies the given function f."
  [f & {:keys [plural]}]
  (fn [x]
    (if (vector? x)
      (if plural
        [x]
        x)
      (f x))))

(defn- put*
  [models {:keys [api]}]
  {:pre [(sequential? models)]}
  (let [prepped (->> models
                     (map (pass-through #(util/+id % (comp str random-uuid))))
                     (mapcat (pass-through deconstruct :plural true))
                     (map (pass-through models->refs))
                     (mapcat prep-for-put)
                     vec)
        {:keys [tempids]} (transact api prepped {})]

    (log/debugf "put models %s" prepped)

    (->> prepped
         (filter (every-pred map?
                             :db/id))
         (map (comp after-read
                    apply-coercions
                    #(util/deep-rename-keys % {:db/id :id})
                    ffirst
                    #(query api {:query '[:find (pull ?x [*])
                                          :in $ ?x]
                                 :args [%]})
                    #(tempids % %)
                    :db/id)))))

; It seems that after an entire entity has been retracted, the id
; can still be returned
(def ^:private naked-id?
  (every-pred map?
              #(= 1 (count %))
              #(= :db/id (first (keys %)))))

(def ^:private id-criterion?
  (every-pred map-entry?
              (comp #(= :id %)
                    first)))

(defn- model-criterion?
  [x]
  (and (map-entry? x)
       (map? (second x))
       (:id (second x))))

(defn- normalize-criterion
  [x]
  (cond
    (id-criterion? x)    (update-in x [1] coerce-id)
    (model-criterion? x) (update-in x [1] select-keys [:id])
    :else x))

(defn- normalize-criteria
  [criteria]
  (postwalk normalize-criterion criteria))

(defn- ->vector
  [x]
  (if (sequential? x)
    (vec x)
    (vector x)))

(defn- extract-model
  [{:keys [select-also]}]
  (let [attributes (->vector select-also)]
    (if (seq attributes)
      (fn [[m & others]]
        (merge m
               (->> others
                    (zipmap attributes)
                    (into {}))))
      first)))

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
      (or (ffirst raw-result) 0)
      (->> raw-result
           (map (extract-model options))
           (remove naked-id?)
           (map (comp after-read
                      apply-coercions
                      #(util/deep-rename-keys % {:db/id :id})))
           (util/apply-sort options)))))

(defn- delete*
  [models {:keys [api] :as opts}]
  {:pre [(and (sequential? models)
              (not-any? nil? models))]}
  (transact api
            (->> models
                 (mapcat #(propagate-delete % opts))
                 (mapv #(vector :db/add (:id %) :model/deleted? true)))
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

^{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn q
  [qry & args]
  (let [api (init-api (get-in env [:db
                                   :strategies
                                   :datomic-peer]))]
    (query api {:query qry :args args})))

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
