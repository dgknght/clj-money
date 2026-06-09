(ns clj-money.service-worker)

(defn register! []
  (when (exists? js/navigator.serviceWorker)
    (-> js/navigator.serviceWorker
        (.register "/service-worker.js" #js {:scope "/"})
        (.then #(.log js/console "SW registered, scope:" (.-scope %)))
        (.catch #(.error js/console "SW registration failed:" %)))))
