(ns clj-money.components
  (:require [reagent.core :as r]
            [cljs-time.core :as t]
            [cljs.core.async :refer [chan <! >! go-loop go]]
            [clj-money.x-platform.util :refer [desc-periodic-seq]]))

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
                         [:span.text-muted @message])})))

(defn- ensure-range
  [start end ranges]
  (if (seq ranges)
    ranges
    [[start end]]))

(defn load-in-chunks
  "Loads data in a series of queries based on a range of dates. This works along-side load-on-scroll
  to provide a stream of items from a large data set.

  :start      - the start of the time period for which data is to be loaded
  :end        - the end of the time period for which data is to be loaded
  :ctl-chan   - an async channel used to trigger the loading of more items
  :fetch-fn   - a fn that will fetch more items based on a date range
  :receive-fn - a fn that will handle the items that were fetched
  :finish-fn  - a fn that will receive notification that no more items are available to be queries"
  [{:keys [start end ctl-chan fetch-fn receive-fn finish-fn interval]
    :or {interval (t/months 1)}}]
  {:pre [start end ctl-chan fetch-fn receive-fn finish-fn interval]}

  (let [items-chan (chan)]

    ; handle items received on the items channel
    (go-loop [call-count 0]
             (when-let [received (<! items-chan)]
               (receive-fn received)
               (recur (inc call-count))))

    ; respond to requests for more items by querying
    ; the service and putting the retrieved items
    ; on the items channel
    (go-loop [date-ranges (->> (desc-periodic-seq start end interval)
                               (partition 2 1)
                               (map (comp #(update-in % [1] t/minus (t/days 1))
                                          vec
                                          reverse))
                               (ensure-range start end)
                               (map #(apply vector :between %)))]
             (let [action (<! ctl-chan) ; action is either :fetch or the minimum number of items we want to fetch before we pause
                   count-needed (if (number? action)
                                  action
                                  50)]
               (when (not= :quit action)
                 (fetch-fn (first date-ranges)
                           #(go
                              (>! items-chan %)
                              (when (< (count %)
                                       count-needed)
                                (>! ctl-chan (- count-needed (count %)))))))
               (if (and (not= :quit action)
                        (seq (rest date-ranges)))
                 (recur (rest date-ranges))
                 (finish-fn))))

    ; Get the first batch
    (go (>! ctl-chan :fetch))))
