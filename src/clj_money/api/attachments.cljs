(ns clj-money.api.attachments
  (:refer-clojure :exclude [update])
  (:require [clojure.core.async :as a]
            [cljs.pprint :refer [pprint]]
            [cljs-http.client :as http]
            [dgknght.app-lib.api-async :as lib-api]
            [clj-money.api :as api :refer [add-error-handler]]
            [clj-money.state :refer [app-state]]))

(defn create
  [{:attachment/keys [transaction] :as attachment}
   & {:as opts
      :keys [on-success callback]
      :or {on-success identity
           callback identity}}]
  {:pre [(:attachment/transaction attachment)]}

  (let [{:keys [on-error]} (add-error-handler opts "Unable to create the attachment: %s")
        ch (http/post (api/path :transactions
                                transaction
                                :attachments)
                      (-> (lib-api/request (assoc opts
                                                  :handle-ex (fn [e]
                                                              (callback)
                                                              (on-error e)
                                                              nil)))
                          (lib-api/multipart-params
                            (dissoc attachment :attachment/transaction))
                          (assoc :oauth-token (:auth-token @app-state))))]
    (a/go
      (when-let [res (a/<! ch)]
        (callback)
        (on-success res)))))

(defn select
  [{:as criteria :attachment/keys [transaction]} & {:as opts}]
  (let [[path crit] (if transaction
                      [(api/path :transactions
                                 (:id transaction)
                                 :attachments)
                       (dissoc criteria :attachment/transaction)]
                      [(api/path :attachments)
                       criteria])]
    (api/get path
             crit 
             (add-error-handler
               opts
               "Unable to retrieve the attachments: %s"))))

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
