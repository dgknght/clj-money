(ns clj-money.images.sql
  (:require [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [next.jdbc :as jdbc]
            [next.jdbc.sql.builder :refer [for-insert for-query]]
            [clj-money.images :as images])
  (:import org.postgresql.util.PSQLException))

(defn- fetch*
  [ds uuid]
  {:pre [uuid]}
  (let [sql (for-query :image_contents
                       {:uuid uuid}
                       jdbc/snake-kebab-opts)]
    (log/debugf "Fetching image %s -> %s" uuid sql)
    (:content (jdbc/execute-one! ds sql jdbc/unqualified-snake-kebab-opts))))

(defn- duplicate-key-ex?
  [e]
  (re-find #"duplicate key value violates unique constraint" (ex-message e)))

(defn- stash*
  [ds uuid content]
  {:pre [uuid content]}
  (try
    (let [sql (for-insert :image_contents
                          {:uuid uuid
                           :content content}
                          jdbc/snake-kebab-opts)]
      (log/debugf "Stashing image %s -> %s" uuid sql)
      (jdbc/execute-one! ds sql)
      uuid)
    (catch PSQLException e
      (if (duplicate-key-ex? e)
        (fetch* ds uuid)
        (throw e)))))

(defmethod images/reify-storage ::images/sql
  [config]
  (let [ds (jdbc/get-datasource config)]
    (reify images/Storage
      (fetch [_ uuid]
        (fetch* ds uuid))
      (stash [_ uuid content]
        (stash* ds uuid content)))))
