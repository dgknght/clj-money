(ns clj-money.entities.cli-auth-sessions
  (:require [java-time.api :as t]
            [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [clj-money.entities :as entities]))

(def ^:private user-code-chars
  "Character set for user codes, excluding confusing characters"
  (remove #{\0 \O \1 \I \l} "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(defn- generate-user-code
  "Generates an 8-character user code using non-confusing characters"
  []
  (string/join (repeatedly 8 #(rand-nth user-code-chars))))

(defn- generate-device-code
  "Generates a high-entropy device code using UUID"
  []
  (str (java.util.UUID/randomUUID)))

(defn- expires-at
  "Returns a timestamp 15 minutes from now"
  []
  (t/plus (t/instant) (t/minutes 15)))

(s/def :cli-auth-session/device-code string?)
(s/def :cli-auth-session/user-code string?)
(s/def :cli-auth-session/user-id (s/nilable int?))
(s/def :cli-auth-session/status #{:pending :approved :denied})
(s/def :cli-auth-session/expires-at inst?)
(s/def :cli-auth-session/created-at inst?)
(s/def :cli-auth-session/approved-at (s/nilable inst?))

(s/def ::entities/cli-auth-session
  (s/keys :req [:cli-auth-session/device-code
                :cli-auth-session/user-code
                :cli-auth-session/status
                :cli-auth-session/expires-at]
          :opt [:cli-auth-session/user-id
                :cli-auth-session/created-at
                :cli-auth-session/approved-at]))

(defmethod entities/before-save :cli-auth-session
  [session]
  (let [now (t/instant)]
    (cond-> session
      (not (:cli-auth-session/device-code session))
      (assoc :cli-auth-session/device-code (generate-device-code))

      (not (:cli-auth-session/user-code session))
      (assoc :cli-auth-session/user-code (generate-user-code))

      (not (:cli-auth-session/status session))
      (assoc :cli-auth-session/status :pending)

      (not (:cli-auth-session/expires-at session))
      (assoc :cli-auth-session/expires-at (expires-at))

      (not (:cli-auth-session/created-at session))
      (assoc :cli-auth-session/created-at now))))

(defn create-session
  "Creates a new CLI authentication session"
  []
  (entities/put {:cli-auth-session/status :pending}))

(defn find-by-device-code
  "Finds a CLI auth session by device code"
  [device-code]
  (entities/find-by {:cli-auth-session/device-code device-code}))

(defn find-by-user-code
  "Finds a CLI auth session by user code"
  [user-code]
  (entities/find-by {:cli-auth-session/user-code user-code}))

(defn expired?
  "Checks if a session has expired"
  [session]
  (t/before? (:cli-auth-session/expires-at session) (t/instant)))

(defn approve-session
  "Approves a CLI auth session for a user"
  [session user]
  (entities/update session
                   {:cli-auth-session/status :approved
                    :cli-auth-session/user-id (:id user)
                    :cli-auth-session/approved-at (t/instant)}))

(defn deny-session
  "Denies a CLI auth session"
  [session]
  (entities/update session
                   {:cli-auth-session/status :denied
                    :cli-auth-session/approved-at (t/instant)}))
