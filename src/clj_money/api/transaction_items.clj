(ns clj-money.api.transaction-items
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :refer [rename-keys]]
            [clojure.spec.alpha :as s]
            [java-time.api :as t]
            [dgknght.app-lib.core :refer [update-in-if
                                          parse-bool
                                          uuid]]
            [dgknght.app-lib.api :as api]
            [clj-money.dates :as dates]
            [clj-money.decimal :as d]
            [clj-money.util :as util]
            [clj-money.accounts :refer [polarizer]]
            [clj-money.comparatives :as comparatives]
            [clj-money.transactions :refer [summarize-items
                                            append-balances]]
            [clj-money.entities :as entities]
            [clj-money.entities.transaction-items :as trx-items]
            [clj-money.entities.accounts :refer [default-commodity]]
            [clj-money.entities.prices :as prices]
            [clj-money.authorization :refer [+scope]]
            [clj-money.authorization.transaction-items]))

(defn- apply-account-recursion
  [{:keys [include-children] :as criteria}]
  (if (parse-bool include-children)
    (update-in criteria
               [:transaction-item/account]
               (fn [id]
                 [:in (map :id (entities/select
                                 {:id id}
                                 :entity-type :account
                                 :include-children? true
                                 :select [:id]))]))

    criteria))

; This could be done at the database layer with more sophisticated
; logic for specifying joins
(defn- filter-reconciled
  [{{:keys [unreconciled]} :params} items]
  (if (parse-bool unreconciled)
    (if-let [recon-ids (->> items
                            (map (comp :id :transaction-item/reconciliation))
                            set
                            seq)]
      (let [unreconciled? (complement
                            (->> (entities/select
                                   {:id [:in recon-ids]
                                    :reconciliation/status :completed})
                                 (map :id)
                                 set))]
        (filter (comp unreconciled?
                      :id
                      :transaction-item/reconciliation)
                items))
      items)
    items))

(defn- unserialize-date
  [x]
  (cond
    (vector? x) (mapv unserialize-date x)
    (string? x) (dates/unserialize-local-date x)
    :else x))

(defn- extract-criteria
  [{:keys [params authenticated]}]
  {:pre [(or (:transaction-date params)
             (:account-id params))]}
  (-> params
      comparatives/symbolize
      (update-in-if [:transaction-date] unserialize-date)
      (rename-keys {:transaction-date :transaction/transaction-date
                    :account-id :transaction-item/account
                    :entity-id :transaction/entity
                    :reconciliation-id :transaction-item/reconciliation})
      apply-account-recursion
      (update-in-if [:transaction-item/account] util/->entity-ref)
      (update-in-if [:transaction-item/reconciliation] (comp util/->entity-ref
                                                         uuid))
      (update-in-if [:transaction/entity] util/->entity-ref)
      (select-keys [:transaction/transaction-date
                    :transaction-item/account
                    :transaction-item/reconciliation
                    :transaction/entity])
      (+scope :transaction-item authenticated)))

(defn- extract-options
  [{:keys [params]}]
  (-> params
      (update-in-if [:limit] parse-long)
      (update-in-if [:skip] parse-long)
      (select-keys [#_:limit :skip])))

; TODO: fix the join problem with the query and move the limit back to the SQL query
(defn- apply-limit
  [{{:keys [limit]} :params} items]
  (if limit
    (take (parse-long limit) items)
    items))

(defn- ->entity-refs
  [items]
  (map #(update-in %
                   [:transaction-item/account]
                   util/->entity-ref)
       items))

(def ^:private no-review-status :none)

(defn- remove-no-review-status
  [item]
  (if (= no-review-status (:transaction/review-status item))
    (dissoc item :transaction/review-status)
    item))

(defn- index
  [req]
  (->> (-> req
           extract-criteria
           (entities/select (assoc (extract-options req)
                                 :sort [[:transaction/transaction-date :desc]
                                        [:transaction-item/index :desc]]
                                 :select-also [:transaction/description
                                               :transaction/transaction-date
                                               :transaction/created-at
                                               :transaction/attachment-count
                                               :transaction/review-status
                                               :transaction-item/transaction]
                                 :nil-replacements {:transaction/attachment-count 0
                                                    ; without a replacement, datomic
                                                    ; leaves out items whose transaction
                                                    ; has no review status
                                                    :transaction/review-status no-review-status})))
       (map remove-no-review-status)
       (filter-reconciled req)
       (apply-limit req)
       ->entity-refs
       api/response))

(s/def ::serialized-date (partial re-matches #"^\d{4}-\d{2}-\d{2}$"))
(s/def ::transaction-date (s/coll-of ::serialized-date :count 2))
(s/def ::account-id ::entities/id)
(s/def ::entity-id ::entities/id)
(s/def ::raw-summary-criteria (s/keys :req-un [::transaction-date]
                                      :opt-un [::account-id
                                               ::entity-id]))

(defn- extract-summary-criteria
  [{:keys [params] :as req}]
  {:pre [(s/valid? ::raw-summary-criteria (:params req))]}
  (-> params
      (rename-keys {:transaction-date :transaction/transaction-date
                    :account-id :transaction-item/account
                    :entity-id :transaction/entity})
      (update-in [:transaction/transaction-date]
                 (fn [dates]
                   (apply vector
                          :between>
                          (map dates/unserialize-local-date dates))))
      (update-in-if [:transaction-item/account] util/->entity-ref)
      (update-in-if [:transaction/entity] util/->entity-ref)
      (select-keys [:transaction-item/account
                    :transaction/transaction-date
                    :transaction/entity])
      ; The account already limits the items to one entity. Matching on the
      ; entity as well makes datomic pair every item in the account with
      ; every transaction in the entity before joining them, which exhausts
      ; the heap for a large entity.
      (cond-> (:account-id params) (dissoc :transaction/entity))))

(s/def ::period-type #{"day" "week" "month" "quarter" "year"})
(s/def ::period-count (partial re-matches #"^\d+$"))
(s/def ::raw-summary-options (s/keys :req-un [::period-type
                                              ::period-count]))

(defn- extract-summary-options
  [{{:keys [period-type period-count] :as params} :params}]
  {:pre [(s/valid? ::raw-summary-options params)]}
  {:period [(parse-long period-count)
            (keyword period-type)]})

(defn- balance-as-of
  "Returns the balance of the account as of the date, or zero if the user
  can't see the account"
  [account as-of authenticated]
  (or (:transaction-item/balance
        (entities/find-by (+scope {:transaction-item/account account
                                   :transaction-item/index [:!= nil]
                                   :transaction/transaction-date [:<= as-of]}
                                  :transaction-item
                                  authenticated)
                          :sort [[:transaction-item/index :desc]]))
      0M))

(defn- apply-balances
  [summaries
   {:transaction-item/keys [account]
    [_ since] :transaction/transaction-date}
   {:keys [params authenticated]}]
  (if (and account (parse-bool (:include-balance params)))
    (append-balances (balance-as-of account
                                    (t/minus since (t/days 1))
                                    authenticated)
                     summaries)
    summaries))

(defn- polarized-value
  [{:transaction-item/keys [value action account]}]
  (d/* (or value d/zero)
       (polarizer action account)))

(defn- activity-value
  "Returns the default-commodity value of the items within the summary's
  period"
  [{:keys [start-date end-date]} items]
  (->> items
       (filter #(dates/within? (:transaction/transaction-date %)
                               start-date
                               end-date))
       (map polarized-value)
       (reduce d/+ d/zero)))

(defn- price-fn
  "Returns a function that takes a date and returns the most recent price of
  the commodity on or before that date, or nil if there isn't one. The prices
  are looked up once for all of the given dates."
  [commodity dates]
  (let [[earliest latest] ((juxt first last) (sort dates))
        prices (->> (entities/select #:price{:commodity commodity
                                             :trade-date [:between
                                                          earliest
                                                          latest]})
                    (cons (prices/most-recent commodity earliest))
                    (filter identity)
                    (sort-by :price/trade-date t/after?))]
    (fn [date]
      (some (fn [{:price/keys [trade-date value]}]
              (when-not (t/after? trade-date date)
                value))
            prices))))

(defn- commodity-values
  "For an account that holds a commodity other than the entity's default,
  adds the value of the activity in each period, which is the cost of the
  shares bought less the cost of the shares sold, and the value of the shares
  held at the end of the period, which is the number of shares times the most
  recent price, or nil if the commodity has no price yet."
  [summaries items {:account/keys [commodity]} as-of]
  (let [last-day (t/minus as-of (t/days 1)) ; as-of is exclusive
        price-date #(t/min (:end-date %) last-day)
        price (price-fn commodity (map price-date summaries))]
    (map (fn [{:keys [balance] :as summary}]
           (cond-> (assoc summary :value (activity-value summary items))
             balance (assoc :balance-value
                            (some->> (price (price-date summary))
                                     (d/* balance)))))
         summaries)))

(defn- apply-values
  "Adds the value, in the entity's default commodity, of the activity in each
  period and, when balances are included, of the balance at the end of each
  period. For an account that tracks the default commodity, these are the
  same as the quantities."
  [summaries
   items
   {:transaction-item/keys [account]
    [_ _ as-of] :transaction/transaction-date}
   {:keys [params authenticated]}]
  (if-let [{:account/keys [commodity entity]
            :as account} (when (and account
                                    (parse-bool (:include-value params)))
                           (entities/find-by (+scope {:id (:id account)}
                                                     :account
                                                     authenticated)))]
    (if (util/id= commodity (default-commodity (entities/find entity)))
      (map (fn [{:keys [quantity balance] :as summary}]
             (cond-> (assoc summary :value quantity)
               balance (assoc :balance-value balance)))
           summaries)
      (commodity-values summaries items account as-of))
    summaries))

(defn- summarize
  [{:keys [authenticated] :as req}]
  (let [{[_ since as-of] :transaction/transaction-date
         :as criteria} (extract-summary-criteria req)
        items (->> (entities/select (+scope criteria
                                            :transaction-item
                                            authenticated)
                                    :select-also [:transaction/transaction-date])
                   trx-items/polarize-quantities)]
    (-> (summarize-items (assoc (extract-summary-options req)
                                :since since
                                :as-of as-of)
                         items)
        (apply-balances criteria req)
        (apply-values items criteria req)
        api/response)))

(def routes
  [["accounts/:account-id/transaction-items" {:get {:handler index}}]
   ["entities/:entity-id/transaction-items/summarize" {:get {:handler summarize}}]])
