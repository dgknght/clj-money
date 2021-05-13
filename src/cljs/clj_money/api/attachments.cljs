(ns clj-money.api.attachments
  (:refer-clojure :exclude [update])
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [<!]]
            [cljs-http.client :as http]
            [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.web :refer [serialize-date]]
            [dgknght.app-lib.api :as api]
            [clj-money.state :refer [app-state]]
            [clj-money.util :as util]))

(defn create
  [attachment success-fn error-fn]
  (go (let [response (<! (http/post (api/path :transactions
                                              (:transaction-id attachment)
                                              (serialize-date (:transaction-date attachment))
                                              :attachments)
                                    (-> {}
                                        (api/multipart-params (dissoc attachment :transaction-id :transaction-date))
                                        (assoc :oauth-token (:auth-token @app-state)))))]
        (if (= 201 (:status response))
          (success-fn (:body response))
          (do
            (.log js/console "Unable to create the attachment" (prn-str response))
            (error-fn (or (some #(% (:body response)) [:dgknght.app-lib.validation/errors
                                                       :error
                                                       :message])
                          (:body response))))))))

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
  [criteria success-fn error-fn]
  (api/get (api/path :attachments)
           (prepare-criteria criteria)
           success-fn
           error-fn))

(defn update
  [attachment success-fn error-fn]
  (api/patch (api/path :attachments
                       (:id attachment))
             attachment
             success-fn
             error-fn))

(defn delete
  [attachment success-fn error-fn]
  (api/delete (api/path :attachments
                        (:id attachment))
              success-fn
              error-fn))
