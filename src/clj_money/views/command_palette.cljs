(ns clj-money.views.command-palette
  "A single input, opened by pressing / when focus isn't already in a
  field, that offers the UI actions matching what's typed into it (issue
  #309), like Gnome Do. The commands, and how they're matched, are in
  clj-money.commands."
  (:require [reagent.core :as r]
            [accountant.core :as accountant]
            [clj-money.state :as state]
            [clj-money.commands :as cmds]))

(def ^:private max-matches
  "The most matches listed, not counting the transaction search that
  always comes last"
  8)

(defonce ^:private palette (r/atom {:open? false}))

(defn open
  "Opens the palette, as pressing / does"
  []
  (reset! palette {:open? true
                   :input ""
                   :active 0}))

(defn- close! []
  (reset! palette {:open? false}))

(defn- typing?
  "True if the key was pressed in a field, where / is typed, not a command"
  [target]
  (or (#{"INPUT" "TEXTAREA" "SELECT"} (.-tagName target))
      (.-isContentEditable target)))

(defn- handle-global-key
  [e]
  (when (and (= "/" (.-key e))
             (not (or (.-ctrlKey e) (.-metaKey e) (.-altKey e)))
             (not (typing? (.-target e)))
             (not (:open? @palette)))
    (.preventDefault e)
    (open)))

(defn- matches
  [items input]
  (let [results (cmds/search (concat (cmds/nav-commands items)
                                     (cmds/account-commands @state/accounts))
                             input)]
    (when (seq results)
      (conj (vec (take max-matches (pop results)))
            (peek results)))))

(defn- execute!
  [{:keys [path]}]
  (close!)
  (accountant/navigate! path))

(defn- option-id
  [index]
  (str "command-palette-option-" index))

(defn- handle-key
  [e options]
  (let [n (count options)
        move (fn [delta]
               (.preventDefault e)
               (when (pos? n)
                 (swap! palette update :active #(mod (+ % delta) n))))]
    (case (.-key e)
      "ArrowDown" (move 1)
      "ArrowUp" (move -1)
      "Escape" (do (.preventDefault e)
                   (close!))
      "Enter" (do (.preventDefault e)
                  (when-let [option (get options (:active @palette))]
                    (execute! option)))
      nil)))

(defn- option
  [index {:keys [label] :as command} active]
  ^{:key (option-id index)}
  [:li.list-group-item.list-group-item-action
   {:id (option-id index)
    :role :option
    :aria-selected (= index active)
    :class (when (= index active) "active")
    ; on mouse down, rather than click, so the input keeps its focus
    :on-mouse-down (fn [e]
                     (.preventDefault e)
                     (execute! command))
    :on-mouse-move #(when-not (= index active)
                      (swap! palette assoc :active index))}
   label])

(defn- dialog
  [items]
  (let [{:keys [input active]} @palette
        options (matches items input)
        expanded? (boolean (seq options))]
    [:div.command-palette-backdrop
     {:on-mouse-down #(when (= (.-target %) (.-currentTarget %))
                        (close!))}
     [:div.command-palette.card.shadow
      {:role :dialog
       :aria-label "Command palette"}
      [:input.form-control.form-control-lg
       {:type :text
        :role :combobox
        :auto-focus true
        :auto-complete :off
        :spell-check false
        :placeholder "Type a page, an account or a description"
        :aria-label "Command"
        :aria-autocomplete :list
        :aria-expanded expanded?
        :aria-controls "command-palette-options"
        :aria-activedescendant (when expanded? (option-id active))
        :value input
        :on-change #(swap! palette assoc
                           :input (.. % -target -value)
                           :active 0)
        :on-key-down #(handle-key % options)
        :on-blur close!}]
      (when expanded?
        [:ul#command-palette-options.list-group.list-group-flush
         {:role :listbox
          :aria-label "Commands"}
         (doall (map-indexed #(option %1 %2 active) options))])]]))

(defn command-palette
  "Listens for / to open the palette, which offers the pages of the nav
  items (as decorated for the navbar) and actions on the cached accounts."
  [_items]
  (r/create-class
    {:display-name "command-palette"
     :component-did-mount
     #(.addEventListener js/document "keydown" handle-global-key)
     :component-will-unmount
     (fn []
       (.removeEventListener js/document "keydown" handle-global-key)
       (close!))
     :reagent-render
     (fn [items]
       (when (:open? @palette)
         [dialog items]))}))
