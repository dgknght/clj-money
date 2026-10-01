(ns clj-money.system
  "Defines the Integrant system for the application.

  The system configuration is built from the application configuration
  (see clj-money.config), so the settings in env/*/config.edn, including
  :config/* references, are used unchanged."
  (:require [integrant.core :as ig]
            [clj-money.config :as config]
            [clj-money.db :as db]
            [clj-money.images :as images]
            [clj-money.progress :as progress]
            [clj-money.web :as-alias web]
            [clj-money.web.handler]
            ; The ::web/server component is defined in clj-money.web.server,
            ; which requires this namespace, so it is loaded by init (via
            ; ig/load-namespaces) rather than required here.
            ; storage strategy implementations
            [clj-money.db.sql]
            [clj-money.db.datomic]
            [clj-money.images.sql]
            [clj-money.images.s3]
            [clj-money.progress.redis]))

(defmethod ig/init-key ::env
  [_ env]
  env)

(defn- port
  [env]
  (let [p (or (:port env) 3000)]
    (if (string? p)
      (parse-long p)
      p)))

(defn config
  "Returns the Integrant configuration map for the system, built from the
  given application configuration (defaults to clj-money.config/env)."
  ([] (config config/env))
  ([env]
   {::env env
    ::db/storage (db/active-config env)
    ::images/storage (:image-storage env)
    ::progress/tracker-factory (progress/active-config env)
    ::web/handler {:env (ig/ref ::env)
                   :storage (ig/ref ::db/storage)
                   :image-storage (ig/ref ::images/storage)
                   :tracker-factory (ig/ref ::progress/tracker-factory)}
    ::web/server {:handler (ig/ref ::web/handler)
                  :port (port env)}}))

(defn init
  "Loads the namespaces for the keys in the given Integrant configuration
  (defaults to the full system configuration) and initializes the system.
  Pass a collection of keys to initialize only those keys and their
  dependencies."
  ([] (init (config)))
  ([cfg]
   (ig/load-namespaces cfg)
   (ig/init cfg))
  ([cfg ks]
   (ig/load-namespaces cfg ks)
   (ig/init cfg ks)))

(defn halt
  "Halts a running system."
  [system]
  (ig/halt! system))
