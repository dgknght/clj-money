(ns clj-money.db.sql.cli-logins
  (:require [java-time.api :as t]
            [clj-money.db.sql :as sql]))

; Timestamps are read as local date-times in the JVM's zone (see
; next.jdbc.date-time/read-as-local in clj-money.db.sql.types)
(defmethod sql/after-read :cli-login
  [login]
  (update-in login [:cli-login/expires-at] #(t/instant (t/zoned-date-time % (t/zone-id)))))
