(ns clj-money.spies
  (:require [clj-money.db :as db]))

(defprotocol Spy
  (calls [this fn-name] "Returns the calls made to the specified function")
  (clear [this] "Clear all data recorded by the spy"))

(defn storage-spy
  [storage]
  (let [calls (atom {})]
    (reify
      db/Storage
      (put [_ entities]
        (swap! calls update-in [:put] (fnil conj []) [entities])
        (db/put storage entities))
      (find [_ id]
        (swap! calls update-in [:find] (fnil conj []) [id])
        (db/find storage id))
      (find-many [_ ids]
        (swap! calls update-in [:find-many] (fnil conj []) [ids])
        (db/find-many storage ids))
      (select [_ criteria options]
        (swap! calls update-in [:select] (fnil conj []) [criteria options])
        (db/select storage criteria options))
      (delete [_ entities]
        (swap! calls update-in [:delete] (fnil conj []) [entities])
        (db/delete storage entities))
      (update [_ changes criteria]
        (swap! calls update-in [:update] (fnil conj []) [changes criteria])
        (db/update storage changes criteria))
      (close [_]
        (swap! calls update-in [:close] (fnil conj []) [])
        (db/close storage))
      (reset [_]
        (swap! calls update-in [:reset] (fnil conj []) [])
        (db/reset storage))

      Spy
      (calls [_ fn-name]
        (@calls fn-name))
      (clear [_]
        (reset! calls {})))))
