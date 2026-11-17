(ns clj-money.mailers
  (:require [clojure.tools.logging :as log]
            [postal.core :refer [send-message]]
            [selmer.parser :refer [render]]
            [clj-money.otel :as otel :refer [with-tracing]]))

(def ^:private send-duration
  (otel/histogram "email.send.duration"
                  :description "Duration of email deliveries"
                  :unit "s"
                  :buckets otel/duration-buckets))

(defn- alt-body
  [parts context]
  (concat
   [:alternative]
   (map #(-> %
             (assoc :content (-> (:template-path %)
                                 slurp
                                 (render context)))
             (dissoc :template-path))
        parts)))

(def ^:private invite-user-parts
  [{:type "text/plain"
    :template-path "resources/templates/mailers/invite_user.txt"}
   {:type "text/html"
    :template-path "resources/templates/mailers/invite_user.html"}])

(defn- invitation-context
  [{:invitation/keys [token invited-by]} {:keys [site-url app-name]}]
  {:sender-full-name (format "%s %s"
                             (:user/first-name invited-by)
                             (:user/last-name invited-by))
   :app-name app-name
   :accept-url (str site-url "/accept-invitation/" token)
   :decline-url (str site-url "/decline-invitation/" token)})

(defn- elapsed-seconds
  [started]
  (/ (- (System/nanoTime) started) 1e9))

(defn- send-and-measure
  "Sends the message, recording the delivery status on the span and the
  duration of the delivery as a metric. Postal reports the outcome of an
  SMTP delivery in the :error key of the result, which is :SUCCESS when
  the message is accepted."
  [span message mailer host]
  (let [attributes {"email.mailer" mailer}
        started (System/nanoTime)]
    (try
      (let [result (send-message {:host host} message)
            status (:error result)]
        (when status
          (otel/set-attribute span :email :delivery :status (name status)))
        (if (or (nil? status) (= :SUCCESS status))
          (otel/record send-duration (elapsed-seconds started) attributes)
          (do
            (otel/set-status span :error (:message result (name status)))
            (otel/record send-duration
                         (elapsed-seconds started)
                         (assoc attributes "error.type" (name status)))
            (log/warnf "Unable to deliver the %s email: %s"
                       mailer
                       (pr-str result))))
        result)
      (catch Exception e
        (otel/record send-duration
                     (elapsed-seconds started)
                     (assoc attributes "error.type" (.getName (class e))))
        (throw e)))))

(defn- deliver-message
  [message mailer {:keys [enabled? host]}]
  (with-tracing [span "email/send"]
    (otel/set-attribute span :email :mailer mailer)
    (otel/set-attribute span :email :enabled (boolean enabled?))
    (if enabled?
      (send-and-measure span message mailer host)
      (log/infof "Mailer disabled. Would have sent: %s" (pr-str message)))))

(defn send-invitation
  "Sends an invitation email. The config is the mailer configuration from
  clj-money.services: :enabled?, :host, :from, :app-name and :site-url."
  [invitation config]
  (deliver-message {:to (:invitation/recipient invitation)
                    :from (:from config)
                    :subject (format "Invitation to %s" (:app-name config))
                    :body (alt-body invite-user-parts
                                    (invitation-context invitation config))}
                   "invitation"
                   config))
