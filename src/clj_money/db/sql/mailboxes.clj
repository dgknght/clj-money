(ns clj-money.db.sql.mailboxes
  (:require [java-time.api :as t]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.db.sql :as sql]))

(defn- ->instant
  "Returns the timestamp read from the database as an instant. It's read as
  a local date-time, in the JVM's time zone."
  [x]
  (if (t/local-date-time? x)
    (t/instant (t/zoned-date-time x (t/zone-id)))
    (.toInstant ^java.util.Date x)))

(defmethod sql/before-save :mailbox
  [mailbox]
  (update-in-if mailbox [:mailbox/checked-at] t/instant->sql-timestamp))

(defmethod sql/after-read :mailbox
  [mailbox]
  (update-in-if mailbox [:mailbox/checked-at] ->instant))
