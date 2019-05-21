(ns clj-money.components
  (:require [reagent.core :as r]))

(defn- debounce
  [timeout f]
  (let [t (atom nil)]
    (fn [& args]
      (when @t (js/clearTimeout @t))
      (reset! t (js/setTimeout (fn []
                                 (reset! t nil)
                                 (apply f args))
                               timeout)))))

(defn load-on-scroll
  [{:keys [target] :as props}]
  (let [message (r/atom "")
        should-load-more? (fn [e]
                            (let [target (.-target e)
                                  scroll-top (.-scrollTop target)
                                  scroll-height (.-scrollHeight target)
                                  client-height (.-clientHeight target)
                                  remaining (- scroll-height (+ scroll-top client-height))
                                  threshold 200]
                              (< remaining threshold)))
        scroll-listener (fn [this e]
                          (let [{:keys [load-fn can-load-more?]} (r/props this)]
                            (if (can-load-more?)
                              (when (should-load-more? e)
                                (reset! message "loading...") ; this never shows because load-fn is asynchronous and returns immediately
                                (load-fn)
                                (reset! message "Items partially loaded."))
                              (reset! message "All items loaded."))))
        debounced-scroll-listener (debounce 200 scroll-listener)
        attach-scroll-listener (fn [this]
                                 (let [targetElem (if target
                                                    (.getElementById js/document target)
                                                    js/window)]
                                   (.addEventListener targetElem "scroll" (partial debounced-scroll-listener this))))
        detach-scroll-listener (fn [this])]
    (r/create-class
      {:component-did-mount (fn [this]
                              (attach-scroll-listener this))
       :component-did-update (fn [this]
                               (attach-scroll-listener this))
       :component-will-unmount (fn [this]
                                 (detach-scroll-listener this))
       :reagent-render (fn [props]
                         [:span @message])})))
