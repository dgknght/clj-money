(ns clj-money.api.attachments
  (:refer-clojure :exclude [update])
  (:require [cljs-http.client :as http]
            [dgknght.app-lib.api-async :as lib-api]
            [clj-money.dates :refer [serialize-local-date]]
            [clj-money.api :as api :refer [add-error-handler]]
            [clj-money.state :refer [app-state]]
            [clj-money.comparatives :as comparatives]))

(defn create
  [{:keys [transaction-id transaction-date] :as attachment} & {:as opts}]
  {:pre [(:transaction-id attachment)
         (:transaction-date attachment)]}

  (http/post (api/path :transactions
                       transaction-id
                       (serialize-local-date transaction-date)
                       :attachments)
             (-> (lib-api/request opts)
                 (lib-api/multipart-params (dissoc attachment
                                                   :transaction-id
                                                   :transaction-date))
                 (add-error-handler "Unable to create the attachment: %s")
                 (assoc :oauth-token (:auth-token @app-state)))))

(defn- prepare-criteria
  [criteria]
  (comparatives/nominalize criteria))

(defn search
  [criteria & {:as opts}]
  (api/get (api/path :attachments)
           (prepare-criteria criteria)
           (add-error-handler
             opts
             "Unable to retrieve the attachments: %s")))

(defn update
  [attachment & {:as opts}]
  (api/patch (api/path :attachments
                       (:id attachment))
             attachment
             (add-error-handler
               opts
               "Unable to update the attachment: %s")))

(defn delete
  [attachment & {:as opts}]
  (api/delete (api/path :attachments
                        (:id attachment))
              (add-error-handler
                opts
                "Unable to delete the attachment: %s")))
