(ns clj-money.models.cached-prices
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [config.core :refer [env]]
            [stowaway.core :refer [tag]]
            [stowaway.implicit :as storage :refer [with-storage
                                                   with-transacted-storage]]
            [dgknght.app-lib.validation :as v :refer [with-validation]]
            [clj-money.models :as models]))

(declare find-by)

(defn- trade-date-unique?
  [cached-price]
  (if (:trade-date cached-price)
    (-> cached-price
        (select-keys [:trade-date :symbol :exchange])
        find-by
        nil?)
    true))
(v/reg-spec trade-date-unique? {:message "%s already exists"
                                :path [:trade-date]})

(s/def ::symbol string?)
(s/def ::trade-date t/local-date?)
(s/def ::price decimal?)
(s/def ::id uuid?)
(s/def ::cached-price (s/and (s/keys :req-un [::trade-date ::price ::exchange ::symbol ::models/exchange])
                             trade-date-unique?))

(defn- after-read
  [cached-price & _]
  (when cached-price
    (-> cached-price
        (tag ::models/cached-price)
        (update-in [:exchange] keyword))))

(defn- prepare-criteria
  [criteria]
  (tag criteria ::models/cached-price))

(defn search
  ([criteria]
   (search criteria {}))
  ([criteria options]
   (with-storage (env :db)
     (->> (storage/select (prepare-criteria criteria) options)
          (map after-read)))))

(defn find-by
  ([criteria]
   (find-by criteria {}))
  ([criteria options]
   (first (search criteria (assoc options :limit 1)))))

(defn find
  ([{:keys [id trade-date]}]
   (find id trade-date))
  ([id trade-date]
   (find-by {:id id
             :trade-date trade-date})))

(defn reload
  [cached-price]
  (find cached-price))

(defn- before-save
  [cached-price]
  (tag cached-price ::models/cached-price))

(defn create
  [cached-price]
  (with-transacted-storage (env :db)
    (with-validation cached-price ::cached-price
      (-> cached-price
          before-save
          storage/create
          after-read))))
