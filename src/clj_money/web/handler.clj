(ns clj-money.web.handler
  "Builds the Ring handler for the application as an Integrant component.

  The handler is built from the component values when the system is
  initialized, rather than when this namespace is loaded, so that
  configuration such as the session secret is only needed at runtime."
  (:require [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [reitit.ring :as ring]
            [reitit.exception :refer [format-exception]]
            [reitit.middleware :as middleware]
            [ring.middleware.defaults :refer [wrap-defaults
                                              site-defaults
                                              api-defaults]]
            [ring.middleware.oauth2 :as oauth2]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.session :refer [wrap-session]]
            [ring.middleware.session.cookie :refer [cookie-store]]
            [integrant.core :as ig]
            [dgknght.app-lib.api :as api]
            [clj-money.otel.web :as otel]
            [clj-money.core]
            [clj-money.db :as db]
            [clj-money.images :as img]
            [clj-money.progress :as prog]
            [clj-money.web :as-alias web]
            [clj-money.decimal :as d]
            [clj-money.web.auth.google :as google-auth]
            [clj-money.web.auth.github :as github-auth]
            [clj-money.web.images :as images]
            [clj-money.middleware :refer [wrap-parse-id-params
                                          wrap-exceptions
                                          wrap-format]]
            [clj-money.entities :as entities]
            [clj-money.db.ref]
            [clj-money.db.sql.ref]
            [clj-money.db.datomic.ref]
            [clj-money.entities.ref]
            [clj-money.api.users :as users-api]
            [clj-money.api.imports :as imports-api]
            [clj-money.api.entities :as entities-api]
            [clj-money.api.commodities :as commodities-api]
            [clj-money.api.prices :as prices-api]
            [clj-money.api.accounts :as accounts-api]
            [clj-money.api.budgets :as budgets-api]
            [clj-money.api.budget-items :as budget-items-api]
            [clj-money.api.reports :as reports-api]
            [clj-money.api.trading :as trading-api]
            [clj-money.api.transactions :as transactions-api]
            [clj-money.api.transaction-items :as transaction-items-api]
            [clj-money.api.scheduled-transactions :as sched-trans-api]
            [clj-money.api.attachments :as att-api]
            [clj-money.api.reconciliations :as recs-api]
            [clj-money.api.lots :as lots-api]
            [clj-money.api.lot-notes :as lot-notes-api]
            [clj-money.api.audit :as audit-api]
            [clj-money.api.invitations :as invitations-api]
            [clj-money.web.users :refer [find-user-by-auth-token]]
            [clj-money.web.apps :as apps]))

(defn- wrap-request-logging
  [handler]
  (fn [{:keys [request-method uri query-string] :as req}]
    (if query-string
      (log/infof "Request %s \"%s?%s\"" request-method uri query-string)
      (log/infof "Request %s \"%s\"" request-method uri))
    (log/debugf "Request details %s \"%s\": %s"
                request-method
                uri
                (with-out-str
                  (pprint
                    (-> req
                        (dissoc :reitit.core/match
                                :reitit.core/router)
                        entities/scrub-sensitive-data))))
    (let [res (handler req)]
      (log/infof "Response %s \"%s\" -> %s" request-method uri (:status res))
      (log/debugf "Response details %s \"%s\": %s"
                  request-method
                  uri
                  (with-out-str
                    (pprint (entities/scrub-sensitive-data res))))
      res)))

(defn- wrap-merge-params
  [handler]
  (fn [{:keys [path-params body-params] :as req}]
    (handler (update-in req [:params] merge path-params body-params))))

(defn- session-store
  [env]
  (cookie-store {:key (.getBytes (or (:session-secret env)
                                     (throw (ex-info "SESSION_SECRET environment variable is required" {}))))}))

(defn- wrap-site []
  [wrap-defaults (-> site-defaults
                     (dissoc :session)
                     (update :security dissoc :anti-forgery))])

(defn- wrap-decimals
  [handler]
  (fn [req]
    (-> req
        handler
        (update-in [:body] d/wrap-decimals))))

(defn- maybe-wrap-oauth2
  [handler env]
  (let [providers (set (:oauth-providers env))
        profiles  (not-empty
                    (merge (when (:google providers) (google-auth/oauth2-profile))
                           (when (:github providers) (github-auth/oauth2-profile))))]
    (if profiles
      (oauth2/wrap-oauth2 handler profiles)
      handler)))

(defn- wrap-components
  "Binds the component values that were supplied, so that requests use the
  storage, image storage and progress tracker factory of the running system.
  Components that are not supplied are left to their existing bindings."
  [handler {:keys [storage image-storage tracker-factory]}]
  (fn [req]
    (binding [db/*storage* (or storage db/*storage*)
              img/*storage* (or image-storage img/*storage*)
              prog/*tracker-factory* (or tracker-factory prog/*tracker-factory*)]
      (handler req))))

(defn router []
  (ring/router ["/" {:middleware [otel/wrap-otel]}
                apps/routes
                ["auth/" {:middleware [:site
                                       wrap-merge-params
                                       wrap-request-logging]}
                 ["google/done" {:get {:handler google-auth/redirect-handler}}]
                 ["github/done" {:get {:handler github-auth/redirect-handler}}]]
                ["app/" {:middleware [:site
                                      wrap-merge-params
                                      wrap-parse-id-params
                                      :authentication
                                      wrap-request-logging]}
                 images/routes]
                ["oapi/" {:middleware [:api
                                       :wrap-format
                                       wrap-decimals
                                       wrap-merge-params
                                       wrap-parse-id-params
                                       wrap-exceptions
                                       wrap-request-logging]}
                 users-api/unauthenticated-routes
                 invitations-api/unauthenticated-routes]
                ["api/" {:middleware [:api
                                      :wrap-format
                                      wrap-decimals
                                      wrap-merge-params
                                      wrap-parse-id-params
                                      :authentication
                                      wrap-exceptions
                                      wrap-request-logging]}
                 users-api/routes
                 entities-api/routes
                 commodities-api/routes
                 accounts-api/routes
                 transactions-api/routes
                 att-api/routes
                 budgets-api/routes
                 budget-items-api/routes
                 imports-api/routes
                 prices-api/routes
                 lots-api/routes
                 lot-notes-api/routes
                 audit-api/routes
                 recs-api/routes
                 reports-api/routes
                 trading-api/routes
                 transaction-items-api/routes
                 sched-trans-api/routes
                 invitations-api/routes]]
               {:conflicts (fn [conflicts]
                             (log/warnf "The application has conflicting routes: %s"
                                        (format-exception :path-conflicts nil conflicts)))
                ::middleware/registry {:site (wrap-site)
                                       :api [wrap-defaults
                                             (-> api-defaults
                                                 (assoc-in [:params :multipart] true)
                                                 (assoc-in [:security :anti-forgery] false))]
                                       :wrap-format wrap-format
                                       :authentication [api/wrap-authentication
                                                        {:authenticate-fn find-user-by-auth-token}]}}))

(defn build
  "Returns the Ring handler for the application.

  Accepts a map with the application config (:env) and, optionally, the
  :storage, :image-storage and :tracker-factory components to bind for each
  request."
  [{:keys [env] :as components}]
  (-> (ring/ring-handler
        (router)
        (ring/routes
          (ring/create-resource-handler {:path "/"})
          apps/spa-fallback
          (ring/create-default-handler)))
      (maybe-wrap-oauth2 env)
      (wrap-session {:store (session-store env)
                     :cookie-attrs {:same-site :lax
                                    :http-only true}})
      wrap-params
      (wrap-components components)))

(defmethod ig/init-key ::web/handler
  [_ components]
  (build components))
