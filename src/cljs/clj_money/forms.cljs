(ns clj-money.forms
  (:require [reagent.core :as r]
            [clojure.string :as string]
            [cljs-time.core :as t]
            [cljs-time.format :as tf]
            [clj-money.util :refer [->id]]
            [clj-money.bootstrap :as bs]
            [clj-money.decimal :refer [->decimal]]
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
  [model field {input-type :type
                :or {input-type :text}}]
  (let [value (r/cursor model field)]
    (fn []
      [:input.form-control {:type input-type
                            :name (->name field)
                            :id (->id field)
                            :value @value
                            :on-change (fn [e]
                                         (let [new-value (.-value (.-target e))]
                                           (swap! model assoc-in field new-value)))}])))

(defn text-field
  [model field options]
  [:div.form-group
   [:label {:for (->id field)} (or (:caption options)
                            (->caption field))]
   [text-input model field options]])

(defn- specialized-text-input
  [model field {input-type :type
                :keys [parse-fn
                       unparse-fn
                       equals-fn
                       icon
                       on-icon-click
                       on-accept]
                :or {input-type :text
                     equals-fn =
                     unparse-fn str
                     on-accept identity}
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
                         :autocomplete :off
                         :name (->name field)
                         :id (->id field)
                         :value @text-value
                         :on-change (fn [e]
                                      (let [new-value (.-value (.-target e))
                                            parsed (parse-fn new-value)]
                                        (when parsed
                                          (swap! model assoc-in field parsed)
                                          (on-accept))
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
           :unparse-fn unparse-date
           :parse-fn parse-date
           :equals-fn t/equal?
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

(defn- parse-decimal
  [value]
  (when (and value
             (re-find #"^-?\d+(\.\d+)?$" value))
    (->decimal value)))

(defn decimal-input
  [model field options]
  [specialized-text-input model field (merge options {:type :number
                                                      :parse-fn parse-decimal})])

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
                           :value (get-in @model field)
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
  [model field {:keys [search-fn
                       find-fn
                       caption-fn
                       value-fn
                       max-items]
                :or {max-items 10}}]
  (let [text-value (r/atom "")
        items (r/atom nil)
        index (r/atom nil)
        select-item #(let [item (when % (nth @items %))
                           [value caption] (if item
                                             ((juxt value-fn caption-fn) item)
                                             [nil ""])]
                       (reset! items nil)
                       (swap! model
                              assoc-in
                              field
                              value)
                       (reset! text-value caption))
        handle-key-down (fn [e]
                          (when @items
                            (case (.-keyCode e)
                              ; up -> 38
                              38 (swap! index (fnil dec (count @items)))

                              ; down -> 40
                              40 (swap! index (fnil inc -1))

                              ; enter -> 13
                              ; tab -> 9
                              (13 9) (select-item @index)

                              ; escape -> 27
                              27 (do
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
                               (when (and v-after
                                          (nil? v-before))
                                 (find-fn v-after #(reset! text-value (caption-fn %)))))))

    (fn []
      [:span
       [:input.form-control {:type :text
                             :auto-complete :off
                             :id (->id field)
                             :name (->name field)
                             :value @text-value
                             :on-key-down handle-key-down
                             :on-change handle-change}]
       [:div.list-group {:style {:z-index 99}}
        (doall (map-indexed (fn [i item]
                              ^{:key (str "option-" (value-fn item))}
                              [:button.list-group-item.list-group-item-action {:type :button
                                                                               :on-click #(select-item i)
                                                                               :class (when (= @index i) "active")}
                               (caption-fn item)])
                            @items))]])))

(defn typeahead-field
  [model field options]
  [:div.form-group
   [:label {:for (->id field)} (or (:caption options)
                                   (->caption field))]
   [typeahead-input model field options]])
