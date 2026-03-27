(ns clj-money.api.invitations
  (:refer-clojure :exclude [update])
  (:require [dgknght.app-lib.web :refer [path]]
            [clj-money.api :as api :refer [add-error-handler]]))

(defn select
  [& {:as opts}]
  (api/get (api/path :invitations)
           {}
           (add-error-handler opts "Unable to retrieve the invitations: %s")))

(defn create
  [invitation opts]
  (api/post (api/path :invitations)
            invitation
            (add-error-handler opts "Unable to create the invitation: %s")))

(defn update
  [invitation opts]
  (api/patch (api/path :invitations invitation)
             (select-keys invitation [:invitation/recipient
                                      :invitation/note])
             (add-error-handler opts "Unable to update the invitation: %s")))

(defn save
  [invitation & {:as opts}]
  (if (:id invitation)
    (update invitation opts)
    (create invitation opts)))

(defn send
  [invitation & {:as opts}]
  (api/post (api/path :invitations invitation :send)
            {}
            (add-error-handler opts "Unable to send the invitation: %s")))

(defn delete
  [invitation & {:as opts}]
  (api/delete (api/path :invitations invitation)
              (add-error-handler opts "Unable to delete the invitation: %s")))

(defn find-by-token
  [token & {:as opts}]
  (api/get (path :oapi :invitations token :accept)
           {}
           (add-error-handler opts "Unable to retrieve the invitation: %s")))

(defn accept
  [token attrs & {:as opts}]
  (api/post (path :oapi :invitations token :accept)
            attrs
            (add-error-handler opts "Unable to accept the invitation: %s")))

(defn decline
  [token & {:as opts}]
  (api/post (path :oapi :invitations token :decline)
            {}
            (add-error-handler opts "Unable to decline the invitation: %s")))
