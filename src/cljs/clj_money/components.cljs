(ns clj-money.components
  (:require [reagent.core :as r]
            [cljs.core.async :as a :refer [chan <! >! go go-loop close!]]
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

(defn- assess-reload
  [count-sought ctl-ch]
  (fn [xf]
    (completing
      (fn [ch items]
        (when (> (swap! count-sought - (count items))
                 0)
          (go (>! ctl-ch :fetch-more)))
        ; When the first call returns more than the count sought,
        ; we put the items here, but they aren't received on the
        ; other end until the :quit command forces it through
        (xf ch items)))))

(defn load-in-chunks
  ([input-seq] (load-in-chunks {} input-seq))
  ([{:keys [chunk-size fetch-xf] :or {chunk-size 50}} input-seq]
   (let [count-sought (atom chunk-size)
         ctl-ch (chan)    ; accepts commands that let criteria past the gate (or shuts everything down)
         fetch-ch (chan 2 ; accepts incoming ranges from the sequence that have passed the gate
                        (comp fetch-xf
                              (assess-reload count-sought ctl-ch))
                        #(.error js/console %))]

     ; Setting the buffer size on the fetch-ch to 1 resulted in the range being passed
     ; directly to the receiver, bypassing the xform. Setting it to 2 resolves that.

     ; If the first result set is larger than the chuck-size, then no data
     ; is ever passed to the receiver. I haven't figured out why, but I'm
     ; pretty sure it's related to the buffer. For now, we'll just need to tune
     ; the range size and chuck size to avoid that

     ; use ctl-ch as a gate for dispatching ranges into the fetch channel
     (go-loop [rng (first input-seq) remaining (rest input-seq)]
              (if rng
                (let [cmd (<! ctl-ch)]
                  (case cmd
                    :fetch (do
                             (reset! count-sought chunk-size)
                             (go (>! fetch-ch rng)))
                    :fetch-more (go (>! fetch-ch rng))
                    :quit (close! fetch-ch))
                  (recur (first remaining) (rest remaining)))
                (do
                  (<! (a/timeout 1000)) ; if the lookup is asynchronous, then we may close the channel before the lookup finishes
                  (close! fetch-ch))))

     ; return the control channel so the caller can prompt more searching,
     ; and the fetch/items channel so they can take receive the items
     {:ctl-ch ctl-ch
      :items-ch fetch-ch})))
