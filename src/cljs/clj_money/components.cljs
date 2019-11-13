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
  "Adds load-on-scroll behavior to a component.
  
  props has the following attributes
    :load-fn                  - the function that loads for items on queue
    :can-load-more?           - a function that returns true if more items are available
    :partially-loaded-content - content to be rendered when the items are partially loaded
    :fully-loaded-content     - content to be displayed when the items are fully loaded
    :loading-content          - content to be dislpayed when the items are actively loading"
  [{:keys [target partially-loaded-content]
    :or {partially-loaded-content "partially loaded"}}]
  (let [message (r/atom partially-loaded-content)
        should-load-more? (fn [e]
                            (let [target (.-target e)
                                  scroll-top (.-scrollTop target)
                                  scroll-height (.-scrollHeight target)
                                  client-height (.-clientHeight target)
                                  remaining (- scroll-height (+ scroll-top client-height))
                                  threshold 200]
                              (< remaining threshold)))
        scroll-listener (fn [this e]
                          (let [{:keys [load-fn
                                        can-load-more?
                                        fully-loaded-content
                                        loading-content]
                                 :or {fully-loaded-content "All items loaded."
                                      loading-content "loading..."}}
                                (r/props this)]
                            (if (can-load-more?)
                              (when (should-load-more? e)
                                (reset! message loading-content)
                                (load-fn)
                                (js/setTimeout #(reset! message partially-loaded-content) 250)) ; a callback would be nice, but complicated. Let's just show a brief message
                              (reset! message fully-loaded-content))))
        debounced-scroll-listener (debounce 200 scroll-listener)
        attach-scroll-listener (fn [this]
                                 (let [targetElem (if target
                                                    (.getElementById js/document target)
                                                    js/window)]
                                   (.addEventListener targetElem "scroll" (partial debounced-scroll-listener this))))
        detach-scroll-listener identity]
    (r/create-class
      {:component-did-mount (fn [this]
                              (attach-scroll-listener this))
       :component-did-update (fn [this]
                               (attach-scroll-listener this))
       :component-will-unmount (fn [this]
                                 (detach-scroll-listener this))
       :reagent-render (fn [_]
                         [:span @message])})))
