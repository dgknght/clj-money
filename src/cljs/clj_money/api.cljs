(ns clj-money.api
  (:refer-clojure :exclude [get])
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

(defn- extract-error
  [{:keys [body]}]
  (or (some #(% body) [:error :message])
      body))

(defn get
  [path success-fn error-fn]
  (go (let [response (<! (http/get path
                                   (append-auth (request))))]
         (if (= 200 (:status response))
           (success-fn (:body response))
           (do
             (.log js/console
                   (str "Unable to get from " path)
                   (prn-str (:body response)))
             (error-fn (extract-error response)))))))

(defn get-resources
  ([path success-fn error-fn]
   (get-resources path {} success-fn error-fn))
  ([path criteria success-fn error-fn]
   (go (let [response (<! (http/get (append-query-string path criteria)
                                    (append-auth (request))))]
         (if (= 200 (:status response))
           (success-fn (:body response))
           (do
             (.log js/console
                   "Unable to get the resource(s) at "
                   path
                   " from the service: "
                   (prn-str (:body response)))
             (error-fn (extract-error response))))))))

(defn get-resource
  ([path success-fn error-fn]
   (get-resource path {} success-fn error-fn))
  ([path criteria success-fn error-fn]
   (get-resource path criteria {} success-fn error-fn))
  ([path criteria options success-fn error-fn]
   (get-resources path criteria options (comp success-fn first) error-fn)))

(defn create-resource
  [path model success-fn error-fn]
  (go (let [response (<! (http/post path (-> (request)
                                             (json-params model)
                                             append-auth)))]
        (case (:status response)
          201 (success-fn (:body response))
          400 (error-fn (get-in response [:body :clj-money.validation/errors]))
          (error-fn (extract-error response))))))

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
