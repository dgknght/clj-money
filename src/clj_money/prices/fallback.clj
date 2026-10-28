(ns clj-money.prices.fallback
  (:require [clojure.tools.logging :as log]
            [clj-money.prices :as prices]))

(defn- fetch-or-nil
  "Fetches the price from the provider at index, returning nil on failure so
  that the next provider can be tried. The failure is logged as a one-line
  warning; the details are left to an observable provider decorating the
  one that failed."
  [[index provider] provider-count {sym :commodity/symbol :as commodity} opts]
  (try
    (prices/fetch provider commodity opts)
    (catch Exception e
      (log/warnf "price provider %s of %s failed for commodity %s: %s"
                 (inc index)
                 provider-count
                 sym
                 (ex-message e))
      nil)))

(defmethod prices/provider ::provider
  [{:keys [providers] :as config}]
  {:pre [(< 0 (count (:providers config)))]}
  (let [ps (mapv #(if (satisfies? prices/Provider %)
                    %
                    (prices/provider %))
                 providers)]
    (reify
      prices/Provider
      (fetch [_ commodity opts]
        (some #(fetch-or-nil % (count ps) commodity opts)
              (map-indexed vector ps))))))
