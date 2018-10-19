(ns clj-money.forms
  (:require-macros [reagent-forms.macros :refer [render-element]])
  (:require [clojure.string :refer [trim]]
            [reagent.core :as r]
            [reagent-forms.core :refer [value-of
                                        default-selection
                                        init-field]]
            [clj-money.inflection :as infl]))

; Start *************************************
; reagent-forms code copied and adjusted here

(defn- scroll-to [element idx]
  (let [list-elem (-> element
                      .-target
                      .-parentNode
                      (.getElementsByTagName "ul")
                      (.item 0))
        idx       (if (< idx 0) 0 idx)
        item-elem (-> list-elem
                      .-children
                      (.item idx))
        [item-height offset-top] (if item-elem
                                   [(.-scrollHeight item-elem)
                                    (.-offsetTop item-elem)]
                                   [0 0])]
    (set! (.-scrollTop list-elem)
          (- offset-top
             (* 2 item-height)))))

(defmethod init-field :my-typeahead
  [[type {:keys [id data-source input-class list-class item-class highlight-class input-placeholder result-fn choice-fn clear-on-focus? selections get-index]
          :as   attrs
          :or   {result-fn       identity
                 choice-fn       identity
                 clear-on-focus? true}}] {:keys [doc get save!]}]
  (let [typeahead-hidden? (r/atom true)
        mouse-on-list?    (r/atom false)
        selected-index    (r/atom -1)
        selections        (or selections (r/atom []))
        get-index         (or get-index (constantly -1))
        choose-selected   #(when (and (not-empty @selections) (> @selected-index -1))
                             (let [choice (nth @selections @selected-index)]
                               (save! id choice)
                               (choice-fn choice)
                               (reset! typeahead-hidden? true)))]
    (render-element attrs doc
                    [type
                     [:input {:type        :text
                              :disabled    (:disabled attrs)
                              :placeholder input-placeholder
                              :class       input-class
                              :value       (let [v (get id)]
                                             (if-not (iterable? v)
                                               v (first v)))
                              :on-focus    #(when clear-on-focus? (save! id nil))
                              :on-blur     #(when-not @mouse-on-list?
                                              (reset! typeahead-hidden? true)
                                              (reset! selected-index -1))
                              :on-change   #(when-let [value (trim (value-of %))]
                                              (reset! selections (data-source (.toLowerCase value)))
                                              (save! id value)
                                              (reset! typeahead-hidden? false)
                                              (reset! selected-index (if (= 1 (count @selections)) 0 -1)))
                              :on-key-down #(do
                                              (case (.-which %)
                                                38 (do
                                                     (.preventDefault %)
                                                     (when-not (or @typeahead-hidden? (<= @selected-index 0))
                                                       (swap! selected-index dec)
                                                       (scroll-to % @selected-index)))
                                                40 (do
                                                     (.preventDefault %)
                                                     (if @typeahead-hidden?
                                                       (do

                                                         (reset! selections (data-source :all))
                                                         (reset! selected-index (get-index (-> %
                                                                                               value-of
                                                                                               trim)
                                                                                           @selections))
                                                         (reset! typeahead-hidden? false)
                                                         (scroll-to % @selected-index))
                                                       (when-not (= @selected-index (dec (count @selections)))
                                                         (save! id (value-of %))
                                                         (swap! selected-index inc)
                                                         (scroll-to % @selected-index))))
                                                9 (choose-selected)
                                                13 (do
                                                     (.preventDefault %)
                                                     (choose-selected))
                                                27 (do (reset! typeahead-hidden? true)
                                                       (reset! selected-index -1))
                                                "default"))}]

                     [:ul {:style          {:display (if (or (empty? @selections) @typeahead-hidden?) :none :block)}
                           :class          list-class
                           :on-mouse-enter #(reset! mouse-on-list? true)
                           :on-mouse-leave #(reset! mouse-on-list? false)}
                      (doall
                        (map-indexed
                          (fn [index result]
                            [:li {:tab-index     index
                                  :key           index
                                  :class         (if (= @selected-index index) highlight-class item-class)
                                  :on-mouse-over #(do
                                                    (reset! selected-index (js/parseInt (.getAttribute (.-target %) "tabIndex"))))
                                  :on-click      #(do
                                                    (.preventDefault %)
                                                    (reset! typeahead-hidden? true)
                                                    (save! id result)
                                                    (choice-fn result))}
                             (result-fn result)])
                          @selections))]])))
; End *************************************

(defn- label
  [field]
  [:label.control-label {:for field} (-> field name infl/last-segment infl/title-case)])

(defn text-input
  [field & validations]
  [:div.form-group
   [label field]
   [:input.form-control {:field :text :id field}]
   (for [validation validations]
       (validation field))])

(defn number-input
  [field & validations]
  [:div.form-group
   [label field]
   [:input.form-control {:field :numeric :id field}]
   (for [validation validations]
       (validation field))])

(defn select-input
  ([field html-options] (select-input field html-options {}))
  ([field html-options options]
   [:div.form-group (merge (select-keys options [:visible?])
                           {:field :container})
   (label field)
   [:select.form-control {:field :list :id field}
    (for [option html-options]
      ^{:key (str (name field) ".option-" option)}
      [:option {:key option} option])]]))

(defn radio-buttons
  [field values]
  [:fieldset
   [:legend (-> field name infl/last-segment infl/title-case)]
   (for [value values]
     ^{:key (str (name field) ".value-" value)}
     [:div.radio
      [:label
       [:input {:field :radio :value value :name field}]
       value]])])

(defn typeahead-input
  "Renders a form field using reagent forms typeahead. Consumers will
  want to specified the following options:
    :data-source - a function that supplies values matching the input
    :in-fn       - a function that accepts an ID and returns a duple containing
                   the display value and the id for a selection
    :out-fn      - a function that takes the duple for the selection and returns
                   the id
    :result-fn   - a function takes the duple for the selection and returns the
                   display value"
  [field options]
  [:div.form-group
   [label field]
   [:div (merge options {:field :my-typeahead
                         :id field
                         :input-class "form-control"
                         :list-class "typeahead-list"
                         :item-class "typeahead-item"
                         :highlight-class "typeahead-highlight"
                         :clear-on-focus? false})]])

(defn required
  [field]
  ^{:key (str (name field) ".required")}
  [:span.help-block {:field :alert :id field :event empty?}
   (str (-> field name infl/last-segment infl/title-case) " is required.")])

(defmethod reagent-forms.core/init-field :list-fn
  [[tag {:keys [list-fn id] :as attr}] {:keys [doc save! get] :as context}]
  (let [options (r/atom [])
        selection (r/atom (or (get id)
                              (get-in (first @options)  [1 :key])))
        elem-attr {:on-focus (fn [_]
                               (reset! options (list-fn doc)))
                   :on-change (fn [selection]
                                (let [option (->> @options
                                                  (filter #(= (value-of selection)
                                                             (-> % second str)))
                                                  first)]
                                  (save! id (second option))))
                   :name id
                   :default-value (default-selection @options @selection)}]
    (render-element attr
                    doc
                    [tag
                     elem-attr
                     (for [[display id] @options]
                       (with-meta [:option {:value id} display]
                                  {:key (str "list-option-" id "-" id)}))])))
