(ns clj-money.web.system
  "Gives request handlers access to the Integrant components of the running
  system, which are assoc'ed to each request by wrap-system.")

(defn wrap-system
  "Assocs the given components (e.g. {:services ...}) to each request, so
  that request handlers can retrieve them with component."
  [handler components]
  (fn [req]
    (handler (assoc req ::system components))))

(defn component
  "Returns the component of the running system with the given key (e.g.
  :services) from the request, or nil if it is not present."
  [req k]
  (get-in req [::system k]))
