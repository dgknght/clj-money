(ns clj-money.api.attachments
  (:refer-clojure :exclude [update])
  (:require [cljs.core.async :as a]
            [cljs-http.client :as http]
            [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.web :refer [serialize-date]]
            [dgknght.app-lib.api-async :as api]
            [clj-money.api :refer [handle-ex]]
            [clj-money.state :refer [app-state]]
            [clj-money.util :as util]))

(defn create
  [{:keys [transaction-id transaction-date] :as attachment} xf]
  {:pre [(:transaction-id attachment)
         (:transaction-date attachment)]}

  (http/post (api/path :transactions
                       transaction-id
                       (serialize-date transaction-date)
                       :attachments)
             (-> {:channel (a/chan 1 xf (handle-ex "Unable to create the attachment: %s"))}
                 (api/multipart-params (dissoc attachment :transaction-id :transaction-date))
                 (assoc :oauth-token (:auth-token @app-state)))))

(defn- serialize-transaction-date
  [criteria]
  (reduce #(update-in-if %1 [%2] serialize-date)
          criteria
          (util/nominative-variations :transaction-date)))

(defn- prepare-criteria
  [criteria]
  (-> criteria
      (util/nominal-comparatives :transaction-date)
      serialize-transaction-date))

(defn search
  [criteria xf]
  (api/get (api/path :attachments)
           (prepare-criteria criteria)
           {:transform xf
            :handle-ex (handle-ex "Unable to retrieve the attachments: %s")}))

(defn update
  [attachment xf]
  (api/patch (api/path :attachments
                       (:id attachment))
             attachment
             {:transform xf
              :handle-ex (handle-ex "Unable to update the attachment: %s")}))

(defn delete
  [attachment xf]
  (api/delete (api/path :attachments
                        (:id attachment))
              {:transform xf
               :handle-ex (handle-ex "Unable to delete the attachment: %s")}))
