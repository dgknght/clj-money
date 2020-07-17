(ns clj-money.api.attachments
  (:refer-clojure :exclude [update])
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [<!]]
            [cljs-http.client :as http]
            [clj-money.api :as api]
            [clj-money.x-platform.util :as util]))

(defn create
  [attachment success-fn error-fn]
  (go (let [response (<! (http/post (api/path :transactions
                                              (:transaction-id attachment)
                                              (util/serialize-date (:transaction-date attachment))
                                              :attachments)
                                    (-> {}
                                        (api/multipart-params (dissoc attachment :transaction-id :transaction-date))
                                        api/append-auth)))]
        (if (= 201 (:status response))
          (success-fn (:body response))
          (do
            (.log js/console "Unable to create the attachment" (prn-str response))
            (error-fn (or (some #(% (:body response)) [:clj-money.validation/errors
                                                       :error
                                                       :message])
                          (:body response))))))))

(defn- serialize-transaction-date
  [criteria]
  (reduce #(util/update-in-if %1 [%2] util/serialize-date)
          criteria
          (util/nominative-variations :transaction-date)))

(defn- prepare-criteria
  [criteria]
  (-> criteria
      (util/nominal-comparatives :transaction-date)
      serialize-transaction-date))

(defn search
  [criteria success-fn error-fn]
  (api/get-resources (api/path :attachments)
                     (prepare-criteria criteria)
                     success-fn
                     error-fn))

(defn update
  [attachment success-fn error-fn]
  (api/update-resource (api/path :attachments
                                 (:id attachment))
                       attachment
                       success-fn
                       error-fn))

(defn delete
  [attachment success-fn error-fn]
  (api/delete-resource (api/path :attachments
                                 (:id attachment))
                       success-fn
                       error-fn))
