(ns clj-money.api.attachments
  (:refer-clojure :exclude [update])
  (:require [cljs.pprint :refer [pprint]]
            [clj-money.api :as api :refer [add-error-handler]]))

(defn create
  [{:attachment/keys [transaction] :as attachment} & {:as opts}]
  {:pre [(:attachment/transaction attachment)]}

  (api/post-multipart (api/path :transactions
                                transaction
                                :attachments)
                      (dissoc attachment :attachment/transaction)
                      (add-error-handler
                        opts
                        "Unable to create the attachment: %s")))

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
