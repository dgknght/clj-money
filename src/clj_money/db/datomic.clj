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
            [clj-money.db.datomic.queries :as queries]
            [clj-money.db.datomic.util :refer [->datoms]]))

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
    :account               '[?x :account/type ?account-type]
    :attachment            '[?x :attachment/image ?attachment-image]
    :budget                '[?x :budget/start-date ?budget-start-date]
    :budget-item           '[?x :budget-item/periods ?budget-item-periods]
    :cached-price          '[?x :cached-price/trade-date ?cached-price-trade-date]
    :commodity             '[?x :commodity/symbol ?commodity-symbol]
    :entity                '[?x :entity/name ?entity-name]
    :grant                 '[?x :grant/user ?grant-user]
    :identity              '[?x :identity/provider ?identity-provider]
    :image                 '[?x :image/uuid ?image-uuid]
    :import                '[?x :import/entity-name ?import-entity-name]
    :lot                   '[?x :lot/purchase-date ?lot-purchase-date]
    :lot-item              '[?x :lot-item/action ?lot-item-action]
    :price                 '[?x :price/value ?price-value]
    :reconciliation        '[?x :reconciliation/status ?reconciliation-status]
    :scheduled-transaction '[?x :scheduled-transaction/description ?scheduled-transaction-description]
    :transaction           '[?x :transaction/description ?transaction-description]
    :transaction-item      '[?x :transaction-item/action ?transaction-item-action]
    :user                  '[?x :user/email ?user-email]))

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
  [criteria {:as opts
             :keys [count
                    nil-replacements]
             :or {nil-replacements {}}}]
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
                                :recursion (recursion opts m-type)
                                :nil-replacements (->java-dates nil-replacements))
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

(defmulti ^:private prep-for-put (fn [m _] (type m)))

(defmethod prep-for-put ::util/map
  [m _api]
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

(def ^:private action-map
  {::db/add :db/add
   ::db/delete :db/retract})

; Here we expact that the datomic transaction has already been constructed
; like the following:
; [:db/add model-id :user/given-name "John"]
;
; Potentially, it could also look like
; [::db/delete {:id 1 :user/given-name "John"}]
; in which case we want to turn it into
; [:db/retract 1]
(defmethod prep-for-put ::util/vector
  [[action & [{:keys [id]} :as args]] api]
  (if (= ::db/delete action)
    (->> (keys (ffirst (query api
                              {:query '[:find (pull ?x [*])
                                        :in $ ?x]
                               :args [id]})))
         (remove #(= :db/id %))
         (map #(vector :db/retract id %)))
    [(apply vector (action-map action action) args)]))

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
                     (mapcat #(prep-for-put % api)))
        {:keys [tempids]} (transact api prepped {})]
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

(def ^:private model-criterion?
  (every-pred map-entry?
              (comp map? second)
              (comp :id second)))

(def ^:private self-reference?
  (every-pred map-entry?
              (comp (partial = "_self")
                    (comp name key))))

(defn- model-in-criterion?
  [x]
  (when (and (map-entry? x)
             (vector? (val x)))
    (let [[oper v] (val x)]
      (and (= :in oper)
           (every? util/model-ref? v)))))

(defn- normalize-criterion
  [x]
  (cond
    (id-criterion? x)       (update-in x [1] coerce-id)
    (model-criterion? x)    (update-in x [1] select-keys [:id])
    (self-reference? x)     (update-in x [1] select-keys [:id])
    (model-in-criterion? x) (update-in x [1] (fn [c]
                                               (update-in c [1] #(->> %
                                                                      (map :id)
                                                                      set))))
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

(defn- apply-limit
  [{:keys [limit]} vs]
  (if limit
    (take limit vs)
    vs))

(defn- select*
  [criteria {:as options :keys [count select]} {:keys [api]}]
  (let [qry (-> criteria
                normalize-criteria
                prepare-criteria
                ->java-dates
                (criteria->query options))
        raw-result (query api qry)]

    (log/debugf "select %s -> %s"
                (models/scrub-sensitive-data criteria)
                qry) ; TODO scrub the datalog query too

    (cond
      count
      (or (ffirst raw-result) 0)

      select
      (map #(zipmap select %) raw-result)

      :else
      (->> raw-result
           (map (extract-model options))
           (remove naked-id?)
           (map (comp after-read
                      apply-coercions
                      #(util/deep-rename-keys % {:db/id :id})))
           (util/apply-sort options)
           (apply-limit options)))))

(defn- single-ns
  [m]
  (let [names (->> (keys m)
                   (map namespace)
                   (into #{}))]
    (if (= 1 (count names))
      (keyword (first names))
      (throw (ex-info "More than one namespace found. Cannot apply the update" m)))))

(defn- update*
  [changes criteria {:keys [api]}]
  (let [target (single-ns changes)
        qry (rearrange-query
              (queries/apply-criteria '{:find [?x]
                                        :in [$]}
                                      criteria
                                      {:target target}))
        updates (->> (query api qry)
                     (map first)
                     (mapcat (fn [id]
                               (->datoms (assoc changes :id id)))))]
    (transact api updates {})))

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
      (update [_ changes criteria] (update* changes criteria {:api api}))
      (close [_])
      (reset [_]            (reset api)))))
