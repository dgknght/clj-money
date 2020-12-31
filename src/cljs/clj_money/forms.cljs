(ns clj-money.forms
  (:require [reagent.core :as r]
            [clojure.string :as string]
            [goog.string :as gstr]
            [cljs-time.core :as t]
            [cljs-time.format :as tf]
            [clj-money.html :refer [key-code
                                    ctrl-key?]]
            [clj-money.util :as util :refer [->id
                                             presence]]
            [clj-money.bootstrap :as bs]
            [clj-money.calendar :as cal]
            [clj-money.views.calendar :as calview]
            [clj-money.inflection :refer [humanize]]))

(defn- ->caption
  [field]
  (let [humanized (humanize (last field))]
    (if-let [trimmed (re-find #"^.+(?= id$)" humanized)]
      trimmed
      humanized)))

(defn- ->name
  [field]
  (->> field
       (map #(if (keyword? %)
               (name %)
               %))
       (string/join "-")))

(defn checkbox-input
  [model field {:keys [on-change] :as options :or {on-change identity}}]
  (let [checked (r/cursor model field)]
    (fn []
      [:input.form-check-input (merge options
                                      {:type :checkbox
                                       :checked @checked
                                       :on-click (fn [e]
                                                   (reset! checked (.-checked (.-target e)))
                                                   (on-change @model))})])))

(defn checkbox-field
  ([model field]
   (checkbox-field model field {}))
  ([model field options]
   (fn []
     (let [id (str (->id field) "-checkbox")]
       [:div.form-check
        [:label.form-check-label {:for id}
         [checkbox-input model field (assoc options :id id)]
         (or (:caption options)
             (->caption field))]]))))

(defn text-input
  [model field {:keys [id on-key-up attr]
                input-type :type
                :or {input-type :text
                     on-key-up identity}}]
  (let [value (r/cursor model field)]
    (fn []
      [:input.form-control (merge attr
                                  {:type input-type
                                   :name (->name field)
                                   :id (or id (->id field))
                                   :value @value
                                   :on-change (fn [e]
                                                (let [new-value (.-value (.-target e))]
                                                  (swap! model assoc-in field new-value)))
                                   :on-key-up on-key-up})])))

(defmulti ^:private nilify
  (fn [model _field]
    (type model)))

(defn- one?
  [coll]
  (= 1 (count coll)))

(defmethod nilify PersistentVector
  [model field]
  (if (one? field)
    (assoc-in model field nil)
    (update-in model (take 1 field) nilify (rest field))))

(defmethod nilify :default
  [model field]
  (if (one? field) 
    (dissoc model (first field))
    (update-in model (take 1 field) nilify (rest field))))

(defn text-field
  [model field options]
  [:div.form-group (merge {} (:form-group-attr options))
   [:label {:for (->id field)} (or (:caption options)
                                   (->caption field))]
   (if (:prepend options)
     [:div.input-group
      [:div.input-group-prepend (:prepend options)]
      [text-input model field options]]
     [text-input model field options])])

(defn- specialized-text-input
  [model field {input-type :type
                :keys [parse-fn
                       unparse-fn
                       equals-fn
                       icon
                       on-icon-click
                       on-accept
                       on-key-up
                       on-key-down
                       id]
                :or {input-type :text
                     equals-fn =
                     unparse-fn str
                     on-accept identity
                     on-key-up identity
                     on-key-down identity}
                :as options}]
  (let [text-value (r/atom (unparse-fn (get-in @model field)))]
    (add-watch model field (fn [_field _sender before after]
                             (let [b (get-in before field)
                                   a (get-in after field)]
                               (when-not (equals-fn a b)
                                 (reset! text-value (unparse-fn a))))))
    (fn []
      (let [attr (merge (select-keys options [:placeholder
                                              :class])
                        {:type input-type
                         :auto-complete :off
                         :name (->name field)
                         :id (or id (->id field))
                         :value @text-value
                         :on-key-down on-key-down
                         :on-key-up on-key-up
                         :on-change (fn [e]
                                      (let [new-value (.-value (.-target e))
                                            parsed (try
                                                     (parse-fn new-value)
                                                     (catch js/Error e
                                                       (.log js/console
                                                             (gstr/format "Error parsing \"%s\": (%s) %s"
                                                                          new-value
                                                                          (.-name e)
                                                                          (.-message e)))
                                                       nil))]
                                        (if (presence new-value)
                                          (when parsed
                                            (swap! model assoc-in field parsed)
                                            (on-accept))
                                          (swap! model nilify field))
                                        (reset! text-value new-value)))})]
        (if icon
          [:div.input-group
           [:input.form-control attr]
           [:div.input-group-append
            [:button.btn.btn-secondary
             {:on-click on-icon-click
              :type :button}
             (bs/icon icon {:size :small})]]]
          [:input.form-control attr])))))

(defn- parse-date
  [date-string]
  (when (and date-string
             (re-find #"^\d{1,2}/\d{1,2}/\d{4}$" date-string))
    (tf/parse (tf/formatter "M/d/yyyy") date-string)))

(defn- unparse-date
  [date]
  (if date
    (tf/unparse (tf/formatter "M/d/yyyy") date)
    ""))

(def ^:private date-input-defaults
  {:unparse-fn unparse-date
   :parse-fn parse-date})

(defn date-input
  [model field options]
  (let [ctl-state (r/atom {:calendar (cal/init {:first-day-of-week :sunday
                                                :selected (get-in @model field)})})
        visible? (r/cursor ctl-state [:visible?])]
    (add-watch model field (fn [k _ before after]
                             (let [b (get-in before k)
                                   a (get-in after k)]
                               (when (and a (nil? b))
                                 (swap! ctl-state assoc
                                        :calendar (cal/init {:first-day-of-week :sunday
                                                             :selected a}))))))
    (fn []
      [:span
       [specialized-text-input
        model
        field
        (merge
          date-input-defaults
          {:on-accept (fn [d]
                        (swap! ctl-state #(-> %
                                              (update-in [:calendar] cal/select d)
                                              (dissoc :visible?))))}
          options
          {:icon :calendar
           :on-key-down (fn [e]
                          (when (ctrl-key? e)
                            (.preventDefault e)
                            (when-let [[oper value] (case (key-code e)
                                                      :left  [t/minus (t/days 1)]
                                                      :right [t/plus  (t/days 1)]
                                                      :up    [t/minus (t/months 1)]
                                                      :down  [t/plus  (t/months 1)]
                                                      nil)]
                              (swap! model update-in field oper value))))
           :unparse-fn unparse-date
           :parse-fn parse-date
           :equals-fn #(when (and %1 %2) (t/equal? %1 %2))
           :on-icon-click #(swap! ctl-state update-in [:visible?] not)})]
       [:div.shadow.rounded {:class (when-not @visible? "d-none")
                             :style {:position :absolute
                                     :border "1px solid var(--dark)"
                                     :padding "2px"
                                     :z-index 99
                                     :background-color "#fff"}}
        [calview/calendar ctl-state {:small? true
                                     :on-day-click (fn [date]
                                                     (swap! model assoc-in field date)
                                                     (swap! ctl-state #(-> %
                                                                           (update-in [:calendar] cal/select date)
                                                                           (dissoc :visible?))))}]]])))

(defn- invalid-feedback
  [model field]
  (get-in model [::invalid-feedback field]))

(defn date-field
  [model field options]
  [:div.form-group
   [:label {:for field} (humanize (last field))]
   [date-input model field options]
   [:div.invalid-feedback (invalid-feedback @model field)]])

(defn- parse-int
  [text-value]
  (when (and text-value
             (re-find #"^\d+$" text-value))
    (js/parseInt text-value)))

(defn integer-input
  [model field options]
  [specialized-text-input model field (merge options {:type :number
                                                      :parse-fn parse-int})])

(defn integer-field
  [model field options]
  [:div.form-group
   [:label {:for (->id field)} (or (:caption options)
                                   (->caption field))]
   [integer-input model field options]])

(defn- parse-float
  [text-value]
  (when (and text-value
             (re-find #"^-?\d+(\.\d+)?$" text-value))
    (js/parseFloat text-value)))

(defn float-input
  [model field options]
  [specialized-text-input model field (merge options {:type :number
                                                      :parse-fn parse-float})])

(defn float-field
  [model field options]
  [:div.form-group
   [:label {:for field} (or (:caption options)
                            (->caption field))]
   [float-input model field options]])

(defn decimal-input
  [model field options]
  [specialized-text-input model field (merge options {:type :text
                                                      :parse-fn util/eval-math})])

(defn decimal-field
  [model field options]
  [:div.form-group
   [:label {:for field} (or (:caption options)
                            (->caption field))]
   [decimal-input model field options]])

(defmulti ^:private select-option
  #(if (vector? %)
     :compound
     (if (keyword? %)
       :keyword
       :simple)))

(defmethod ^:private select-option :keyword
  [item field]
  ^{:key (str "option-" field "-" (name item))}
  [:option {:value (name item)} (name item)])

(defmethod ^:private select-option :simple
  [item field]
  ^{:key (str "option-" field "-" (str item))}
  [:option {:value (str item)} (str item)])

(defmethod ^:private select-option :compound
  [[value caption] field]
  ^{:key (str "option-" field "-" value)}
  [:option {:value value} caption])

(defn select-elem
  [model field items _options]
  (fn []
    [:select.form-control {:id field
                           :name field
                           :value (or (get-in @model field) "")
                           :on-change (fn [e]
                                        (let [value (.-value (.-target e))]
                                          (swap! model assoc-in field (if (empty? value)
                                                                        nil
                                                                        value))))}
     (->> (if (coll? items)
            items
            @items)
          (map #(select-option % field))
          doall)]))

(defn select-field
  ([model field items] (select-field model field items {}))
  ([model field items options]
   [:div.form-group (select-keys options [:class])
    [:label {:for (->id field)} (or (:caption options)
                                    (->caption field))]
    [select-elem model field items options]]))

(defn typeahead-input
  "Renders an input field with typeahead search capability

  model - an atom wrapping a map which contains an attribute to be updated
  field - a vector of fields identifying the attribute to be updated (as is get-in/update-in)
  options -
  search-fn       - a fn that takes a single string argument and returns matching data records
  find-fn         - a fn that takes the stored values and finds the corrsponding data record
  caption-fn      - accepts a data record and returns the value to display in the field
  list-caption-fn - like caption-fn, but used to render a data record in the list. Uses caption-fn if not supplied
  value-fn        - accepts a data record and returns the value to be stored in the attribute
  on-change       - callback invoked when the value of the attribute changes
  on-key-up       - callback invoked on key-up
  max-items       - the maximum number of matching data records to show in the list
  list-attr       - attributes to be applied to the list HTML element"
  [model field {:keys [search-fn
                       find-fn
                       caption-fn
                       list-caption-fn
                       value-fn
                       on-change
                       on-key-up
                       list-attr
                       max-items
                       id]
                :or {max-items 10
                     on-change identity
                     on-key-up identity}
                :as options}]
  (let [text-value (r/atom "")
        items (r/atom nil)
        index (r/atom nil)
        list-caption-fn (or list-caption-fn caption-fn)
        select-item (fn [index]
                      (let [item (when index (nth @items index))
                            [value caption] (if item
                                              ((juxt value-fn caption-fn) item)
                                              [nil ""])]
                        (reset! items nil)
                        (swap! model
                               assoc-in
                               field
                               value)
                        (on-change item)
                        (reset! text-value caption)))
        handle-key-down (fn [e]
                          (when @items
                            (case (key-code e)
                              :up           (swap! index #(-> (or % (count @items))
                                                              dec
                                                              (mod (count @items))))
                              :down         (swap! index #(-> (or % -1)
                                                              inc
                                                              (mod (count @items))))
                              (:enter :tab) (select-item @index)
                              :escape       (do
                                              (find-fn (get-in @model field)
                                                       #(reset! text-value (caption-fn %)))
                                              (reset! items nil))

                              nil)))
        handle-change (fn [e]
                        ; TODO: debounce this lookup
                        (let [raw-value (.-value (.-target e))]
                          (reset! text-value raw-value)
                          (if (empty? raw-value)
                            (reset! items nil)
                            (search-fn raw-value #(->> %
                                                       (take max-items)
                                                       (reset! items))))))]
    (when-let [current-value (get-in @model field)]
      (find-fn current-value #(reset! text-value (caption-fn %))))
    (add-watch model field (fn [_field _sender before after]
                             (let [v-before (get-in before field)
                                   v-after (get-in after field)]
                               (if v-after
                                 (when (nil? v-before)
                                   (find-fn v-after #(reset! text-value (caption-fn %))))
                                 (reset! text-value "")))))

    (fn []
      (let [input [:input.form-control {:type :text
                                        :auto-complete :off
                                        :id (or id (->id field))
                                        :name (->name field)
                                        :value @text-value
                                        :on-key-down handle-key-down
                                        :on-key-up #(when-not @items (on-key-up %))
                                        :on-change handle-change}]
            result-list [:div.list-group (merge-with merge
                                                     {:style {:z-index 99
                                                              :position "absolute"}}
                                                     list-attr)
                         (doall (map-indexed (fn [i item]
                                               ^{:key (str (string/join field "-") "option-" i)}
                                               [:button.list-group-item.list-group-item-action {:type :button
                                                                                                :on-click #(select-item i)
                                                                                                :class (when (= @index i) "active")}
                                                (list-caption-fn item)])
                                             @items))]]
        (if (:prepend options)
          [:div.input-group
           [:div.input-group-prepend
            (:prepend options)]
           input
           result-list]
          [:span
           input
           result-list])))))

(defn typeahead-field
  [model field options]
  [:div.form-group (merge {} (:form-group-attr options))
   [:label {:for (->id field)} (or (:caption options)
                                   (->caption field))]
   [typeahead-input model field options]])
