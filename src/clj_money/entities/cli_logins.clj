(ns clj-money.entities.cli-logins
  "A pending sign-in from the command-line app. The CLI holds the device
  code, which is secret, and polls with it; the user sees the user code in
  the browser and approves the login there, which links it to them. See
  clj-money.cli-logins."
  (:require [clojure.spec.alpha :as s]
            [clj-money.entities :as entities]))

(s/def :cli-login/device-code (s/and string? #(<= 32 (count %) 64)))
(s/def :cli-login/user-code (s/and string? #(re-matches #"[A-Z]{4}-[A-Z]{4}" %)))
(s/def :cli-login/user (s/nilable ::entities/entity-ref))
(s/def :cli-login/expires-at inst?)

(s/def ::entities/cli-login
  (s/keys :req [:cli-login/device-code
                :cli-login/user-code
                :cli-login/expires-at]
          :opt [:cli-login/user]))
