(ns clj-money.authorization.transfers
  (:require [clj-money.authorization :as authorization]
            [clj-money.entities :as entities]
            [clj-money.entities.auth-helpers :refer [owner-or-granted?]]))

; A transfer changes both of its accounts, so the user must be allowed the
; action on each of them
(defmethod authorization/allowed? [:transfer ::authorization/manage]
  [{:transfer/keys [from-account to-account]} action user]
  (every? #(when-let [account (entities/find %)]
             (owner-or-granted? account user action))
          [from-account to-account]))
