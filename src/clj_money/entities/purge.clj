(ns clj-money.entities.purge
  (:require [clojure.set :as set]
            [clj-money.db :as db]
            [clj-money.entities :as entities]
            [clj-money.db.datomic :as datomic]))

(defn- gather-ids
  "Breadth-first collection of every id that transitively depends on the
  given entity id, keyed by entity type, using clj-money.db.datomic/dependent-attrs.
  Sets make this safe against cycles (e.g. an account's self-referential
  :parent ref)."
  [entity-id]
  (loop [frontier {:entity #{entity-id}}
         acc      {:entity #{entity-id}}]
    (let [found (reduce-kv
                  (fn [m type ids]
                    (reduce (fn [m2 [owner-type attr]]
                              (update m2
                                      owner-type
                                      (fnil set/union #{})
                                      (set (map :id (entities/select {attr [:in ids]})))))
                            m
                            (datomic/dependent-attrs type)))
                  {}
                  frontier)
          new-frontier (reduce-kv
                         (fn [m type ids]
                           (let [unseen (set/difference ids (get acc type #{}))]
                             (cond-> m (seq unseen) (assoc type unseen))))
                         {}
                         found)]
      (if (empty? new-frontier)
        acc
        (recur new-frontier (merge-with set/union acc new-frontier))))))

(defn purge-entity!
  "Completely and permanently removes an entity and everything that
  transitively depends on it (accounts, transactions, budgets, etc.).
  Unlike a normal delete, this is irreversible - there is no soft-delete or
  deactivation option here.

  On the SQL backend this simply deletes the entity row; the rest cascades
  via foreign key constraints. On the Datomic backend, there is no cascading
  delete, and a plain retraction would leave the full history in place, so
  every dependent id is gathered and excised."
  [entity]
  (if (#{:clj-money.db/datomic-peer :clj-money.db/datomic-client} (db/active-strategy))
    (datomic/excise! (mapcat val (gather-ids (:id entity))))
    (entities/delete entity)))
