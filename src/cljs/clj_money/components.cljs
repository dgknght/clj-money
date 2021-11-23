(ns clj-money.components
  (:require [reagent.core :as r]
            [cljs.core.async :refer [chan <! >! go-loop go close!]]
            [clj-money.util
             :as util
             :refer [debounce]]))

(defn load-on-scroll
  "Adds load-on-scroll behavior to a component.

  props has the following attributes
    :load-fn                  - the function that loads for items on queue
    :all-items-fetched?       - a dereferencable item indicating whether or not all items have been fetched
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
                                        all-items-fetched?
                                        fully-loaded-content
                                        loading-content]
                                 :or {fully-loaded-content "All items loaded."
                                      loading-content "loading..."}}
                                (r/props this)]
                            (if all-items-fetched?
                              (reset! message fully-loaded-content)
                              (when (should-load-more? e)
                                (reset! message loading-content)
                                (load-fn)
                                (js/setTimeout #(reset! message (if all-items-fetched?
                                                                  fully-loaded-content
                                                                  partially-loaded-content))
                                               250))))) ; a callback would be nice, but complicated. Let's just show a brief message)))
        debounced-scroll-listener (debounce 200 scroll-listener)
        attach-scroll-listener (fn [this]
                                 (let [targetElem (if target
                                                    (.getElementById js/document target)
                                                    js/window)]
                                   (.addEventListener targetElem "scroll" (partial debounced-scroll-listener this))))
        detach-scroll-listener identity]
    (r/create-class
     {:display-name "load-on-scroll"
      :component-did-mount (fn [this]
                             (attach-scroll-listener this))
      :component-did-update (fn [this]
                              (attach-scroll-listener this))
      :component-will-unmount (fn [this]
                                (detach-scroll-listener this))
      :reagent-render (fn [_]
                        [:span.text-muted @message])})))

(defn load-in-chunks
  ([input-seq] (load-in-chunks {} input-seq))
  ([{:keys [chunk-size] :or {chunk-size 50}} input-seq]
   (let [fetch-ch (chan)  ; accepts incoming search criteria that has passed the gate
         result-ch (chan) ; accepts results of searches based on criteria
         ctl-ch (chan)    ; accepts commands that let criteria past the gate (or shuts everything down)
         count-sought (atom nil)]

     ; use ctl-ch as a gate for dispatching ranges into the fetch channel
     (go
       (loop [rng (first input-seq) remaining (rest input-seq)]
         (if rng
           (let [cmd (<! ctl-ch)]
             (case cmd
               :fetch (do
                        (reset! count-sought chunk-size)
                        (go (>! fetch-ch rng)))
               :fetch-more  (go (>! fetch-ch rng))
               :quit (close! fetch-ch)))
           (close! fetch-ch))
         (when (seq remaining)
           (recur (first remaining) (rest remaining)))))

     ; take the search results and update the count still sought
     (go-loop [items (<! result-ch)]
              (when items
                (swap! count-sought - (count items))
                (when (> @count-sought 0)
                  (go (>! ctl-ch :fetch-more)))
                (recur (<! result-ch))))
     
     ; return the channels so the caller can participate
     {:fetch-ch fetch-ch
      :result-ch result-ch
      :ctl-ch ctl-ch})))
