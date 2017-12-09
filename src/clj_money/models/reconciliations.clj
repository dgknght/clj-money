(ns clj-money.models.reconciliations
  (:refer-clojure :exclude [update])
  (:require [clojure.spec :as s]
            [clojure.pprint :refer [pprint]]
            [clj-time.coerce :refer [to-long
                                     to-local-date]]
            [clj-money.util :refer [parse-local-date]]
            [clj-money.validation :as validation]
            [clj-money.coercion :as coercion]
            [clj-money.authorization :as authorization]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.transactions :as transactions]
            [clj-money.models.helpers :refer [with-storage
                                              with-transacted-storage
                                              create-fn
                                              update-fn]]
            [clj-money.models.storage :refer [create-reconciliation
                                              update-reconciliation
                                              select-reconciliations
                                              set-transaction-item-reconciled
                                              unreconcile-transaction-items-by-reconciliation-id
                                              delete-reconciliation]])
  (:import org.joda.time.LocalDate
           java.util.UUID))

(s/def ::account-id integer?)
(s/def ::end-of-period #(instance? LocalDate %))
(s/def ::balance decimal?)
(s/def ::status #{:new :completed})
(s/def ::item-id (s/tuple uuid? #(instance? LocalDate %)))
(s/def ::item-ids (s/coll-of ::item-id))

(s/def ::new-reconciliation (s/keys :req-un [::account-id ::end-of-period ::status ::balance] :opt-un [::item-ids]))
(s/def ::existing-reconciliation (s/keys :req-un [::id ::end-of-period ::status ::balance] :opt-un [::account-id ::item-ids]))

(coercion/register-coerce-fn
  :transaction-item-refs
  (fn [values]
    (map (fn [value]
           (if (sequential? value)
             (let [[id date] value]
               [(if (uuid? id)
                  id
                  (UUID/fromString id))
                (if (instance? LocalDate date)
                  date
                  (parse-local-date date))])
             value))
         values)))

(def ^:private coercion-rules
  [(coercion/rule :local-date [:end-of-period])
   (coercion/rule :decimal [:balance])
   (coercion/rule :integer [:account-id])
   (coercion/rule :integer [:id])
   (coercion/rule :transaction-item-refs [:item-ids])])

(defn- before-validation
  ([reconciliation]
   (before-validation nil reconciliation))
  ([_ reconciliation]
   (-> (coercion/coerce coercion-rules reconciliation)
       (update-in [:status] (fnil identity :new)))))

(defn- before-save
  ([reconciliation]
   (before-save nil reconciliation))
  ([_ reconciliation]
   (-> reconciliation
       (update-in [:status] name))))

(defn- append-transaction-item-refs
  [reconciliation storage]
  (when reconciliation
    (assoc reconciliation
           :item-ids
           (mapv (juxt :id :transaction-date)
                 (transactions/select-items-by-reconciliation
                       storage
                       reconciliation)))))

(defn- after-read
  ([reconciliation]
   (after-read nil reconciliation))
  ([storage reconciliation]
   (when reconciliation
     (-> reconciliation
         (update-in [:status] keyword)
         (authorization/tag-resource :reconciliation)
         (append-transaction-item-refs storage)))))

(defn search
  ([storage-spec criteria]
   (search storage-spec criteria {}))
  ([storage-spec criteria options]
   (with-storage [s storage-spec]
     (map #(after-read s %) (select-reconciliations s criteria options)))))

(defn find
  [storage-spec criteria]
  (first (search storage-spec criteria {:limit 1})))

(defn find-last-completed
  "Returns the last reconciled balance for an account"
  [storage-spec account-id]
  (search storage-spec
          {:account-id account-id
           :status :completed}
          {:limit 1
           :sort [[:end-of-period :desc]]}))

; TODO this still isn't ensureing that they are only loaded once, need to rework it
(defn- ensure-transaction-items
  [storage {item-ids :item-ids :as reconciliation}]
  (let [ids (map first item-ids)
        date-range ((juxt first last) (sort (map second item-ids)))]
    (update-in reconciliation
               [::items]
               (fnil identity (if item-ids
                                (transactions/find-items-by-ids
                                  storage
                                  ids
                                  date-range)
                                [])))))

(defn find-by-id
  "Returns the specified reconciliation"
  [storage-spec id]
  (first (search storage-spec {:id id} {:limit 1})))

(defn- is-in-balance?
  [storage reconciliation]
  (or (= :new (:status reconciliation))
      (let [account (accounts/find-by-id storage (:account-id reconciliation))
            starting-balance (or (:balance (find-last-completed storage (:account-id reconciliation)))
                                 0M)
            delta (->> reconciliation
                       (ensure-transaction-items storage)
                       ::items
                       (map #(accounts/polarize-amount % account))
                       (reduce +))]
        (= (:balance reconciliation)
           (+ starting-balance delta)))))

(defn- items-belong-to-account?
  [storage {account-id :account-id :as reconciliation}]
  (or (nil? (:item-ids reconciliation))
      (= (->> reconciliation
              (ensure-transaction-items storage)
              ::items
              (map :account-id)
              set)
         #{account-id})))

(defn- items-do-not-belong-to-another-reconciliation?
  [storage {id :id :as reconciliation}]
  (let [reconciliation-ids (->> reconciliation
                                (ensure-transaction-items storage)
                                ::items
                                (map :reconciliation-id)
                                (filter identity)
                                set)]
    (or (empty? reconciliation-ids)
        (= reconciliation-ids #{id}))))

(defn- can-be-updated?
  [storage {:keys [status id]}]
  (or (nil? id)
      (= :new (:status (find-by-id storage id)))))

(defn- is-after-last-reconciliation?
  [storage reconciliation]
  (let [last-completed (find-last-completed storage (:account-id reconciliation))]
    (or (nil? last-completed)
        (> 0 (compare (:end-of-period last-completed)
                      (:end-of-period reconciliation))))))

(defn find-working
  "Returns the uncompleted reconciliation for the specified
  account, if one exists"
  [storage-spec account-id]
  (find storage-spec
        {:status "new"
         :account-id account-id}))

(defn- working-reconciliation-exists?
  [storage {:keys [account-id id] :as rec}]
  (when account-id
    (when-let [existing (find-working storage account-id)]
      (or (nil? id) (not= id (:id existing))))))

(defn- no-working-reconciliation-exists?
  [storage reconciliation]
  (not (working-reconciliation-exists? storage reconciliation)))

(defn- validation-rules
  [storage]
  [(validation/create-rule (partial is-in-balance? storage)
                           [:balance]
                           "The account balance must match the statement balance.")
   (validation/create-rule (partial items-belong-to-account? storage)
                           [:item-ids]
                           "All items must belong to the account being reconciled")
   (validation/create-rule (partial items-do-not-belong-to-another-reconciliation? storage)
                           [:item-ids]
                           "No items may belong to another reconcilidation")
   (validation/create-rule (partial is-after-last-reconciliation? storage)
                           [:end-of-period]
                           "End of period must be after the latest reconciliation")
   (validation/create-rule (partial can-be-updated? storage)
                           [:status]
                           "A completed reconciliation cannot be updated")
   (validation/create-rule (partial no-working-reconciliation-exists? storage)
                           [:account-id]
                           "A new reconciliation cannot be created while a working reconciliation already exists")])

(defn- validate
  [spec rules reconciliation]
  (->> reconciliation
       before-validation
       (validation/validate spec rules)))

(defn- after-save
  [storage {item-refs :item-ids :as reconciliation}]
  ; Set reconciled flag on specified transaction items
  (when (and item-refs (seq item-refs))
    (doseq [[item-id transaction-date] item-refs]
      (set-transaction-item-reconciled storage
                                       (:id reconciliation)
                                       item-id 
                                       transaction-date)))
  reconciliation)

(defn- create*
  [storage reconciliation]
  (merge reconciliation
  (create-reconciliation storage reconciliation)))

(def create
  (create-fn {:spec ::new-reconciliation
              :before-validation before-validation
              :create create*
              :before-save before-save
              :after-save after-save
              :rules-fn validation-rules
              :after-read after-read
              :coercion-rules coercion-rules}))

(defn reload
  "Returns the same reconciliation reloaded from the data store"
  [storage-spec {id :id}]
  (find-by-id storage-spec id))

(defn- set-account-id
  [storage reconciliation]
  (let [existing (find-by-id storage (Integer. (:id reconciliation)))]
    (assoc reconciliation :account-id (:account-id existing))))

(defn update
  "Updates the specified reconciliation"
  [storage-spec reconciliation]
  (with-storage [s storage-spec]
    (let [validated (->> reconciliation
                         (set-account-id s)
                         (validate ::existing-reconciliation (validation-rules s)))]
      (if (validation/valid? validated)
        (do
          (->> validated
               before-save
               (update-reconciliation s))
          (unreconcile-transaction-items-by-reconciliation-id s (:id validated))
          (when (and (:item-ids validated) (seq (:item-ids validated))) ; TODO: remove this redundancy wth create
            (doseq [[item-id date] (:item-ids validated)]
              (set-transaction-item-reconciled s (:id validated) item-id date)))
          (reload s validated))
        validated))))

(defn delete
  "Removes the specified reconciliation from the system. (Only the most recent may be deleted.)"
  [storage-spec id]
  (with-transacted-storage [s storage-spec]
    (let [reconciliation (find-by-id s id)
          most-recent (first (search s
                                     {:account-id (:account-id reconciliation)}
                                     {:sort [[:end-of-period :desc]]
                                      :limit 1}))]
      (when (not= id (:id most-recent))
        (throw (ex-info "Only the most recent reconciliation may be deleted" {:specified-reconciliation reconciliation
                                                                              :most-recent-reconciliation most-recent})))
      (unreconcile-transaction-items-by-reconciliation-id s id)
      (delete-reconciliation s id))))
