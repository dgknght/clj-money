(ns clj-money.api
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [<!]]
            [cljs-http.client :as http]
            [clj-money.x-platform.util :refer [map->query-string]]
            [clj-money.util :as util]
            [clj-money.state :refer [app-state]]))

(defn path
  [& segments]
  (apply util/path (concat [:api] segments)))

(defn- append-query-string
  [path criteria]
  (if (empty? criteria)
    path
    (let [query-string (map->query-string criteria)]
      (str path "?" query-string))))

(defn header
  [req k v]
  (update-in (or req {}) [:headers] assoc k v))

(defn content-type
  [req content-type]
  (header req "Content-Type" content-type))

(defn accept
  [req content-type]
  (header req "Accept" content-type))

(defn append-auth
  [req]
  (if-let [token (get-in @app-state [:auth-token])]
    (header req "Authorization" (str "Bearer " token))
    req))

(defn json-params
  [req params]
  (assoc req :json-params params))

(defn multipart-params
  [req params]
  (assoc req :multipart-params params))

(defn request []
  (-> {}
      (content-type "application/json")
      (accept "application/json")))

(defn get-resources
  ([path success-fn error-fn]
   (get-resources path {} success-fn error-fn))
  ([path criteria success-fn error-fn]
   (get-resources path criteria {} success-fn error-fn))
  ([path criteria options success-fn error-fn]
   (go (let [params (cond-> {:criteria criteria}
                      (seq options)
                      (assoc :options options))
             response (<! (http/get (append-query-string path params)
                                    (append-auth (request))))]
         (if (= 200 (:status response))
           (success-fn (:body response))
           (do
             (.log js/console
                   "Unable to get the resource(s) at "
                   path
                   " from the service: "
                   (prn-str (:body response)))
             (error-fn (-> response :body :message))))))))

(defn get-resource
  ([path success-fn error-fn]
   (get-resource path {} success-fn error-fn))
  ([path criteria success-fn error-fn]
   (get-resource path criteria {} success-fn error-fn))
  ([path criteria options success-fn error-fn]
   (get-resources path criteria options (comp success-fn first) error-fn)))

(defn- extract-error
  [response]
  (some #(% response)
        [(comp :message :body)
         :error-text]))

(defn create-resource
  [path model success-fn error-fn]
  (go (let [response (<! (http/post path (-> (request)
                                             (json-params model)
                                             append-auth)))]
        (if (= 201 (:status response))
          (success-fn (:body response))
          (do
            (.log js/console (prn-str {:unable-to-create model
                                       :response response}))
            (error-fn (extract-error response)))))))

(defn update-resource
  [path model success-fn error-fn]
  (go (let [response (<! (http/patch path (-> (request)
                                              (json-params model)
                                              append-auth)))]
        (if (= 200 (:status response))
          (success-fn (:body response))
          (do
            (.log js/console "Unable to update the model " (prn-str model) ": " (prn-str response))
            (error-fn (extract-error response)))))))

(defn delete-resource
  [path success-fn error-fn]
  (go (let [response (<! (http/delete path (append-auth (request))))]
        (case (:status response)
          204
          (success-fn)
          404
          (error-fn "The specified resource could not be found on the server.")

          (error-fn (-> response :body :message))))))
