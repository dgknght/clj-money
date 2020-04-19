(ns clj-money.plain-forms
  (:require [reagent.core :as r]
            [clojure.string :as string]
            [cljs-time.format :as tf]
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

(defn- ->id
  [field]
  (->name field))

(defn checkbox-input
  [model field options]
  (let [checked (r/cursor model field)]
    (fn []
      [:input.form-check-input (merge options
                                      {:type :checkbox
                                       :checked @checked
                                       :on-change (fn [e]
                                                    (reset! checked (.-checked (.-target e))))})])))

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
                parse-fn :parse-fn
                unparse-fn :unparse-fn
                :or {input-type :text
                     unparse-fn str}}]
  (let [text-value (r/atom (unparse-fn (get-in @model field)))]
    (add-watch model field (fn [_field _sender before after]
                             (let [b (get-in before field)
                                   a (get-in after field)]
                               (when (and a (not b))
                                 (reset! text-value (unparse-fn a))))))
    (fn []
      [:input.form-control {:type input-type
                            :name (->name field)
                            :id (->id field)
                            :value @text-value
                            :on-change (fn [e]
                                         (let [new-value (.-value (.-target e))
                                               parsed (parse-fn new-value)]
                                           (when parsed
                                             (swap! model assoc-in field parsed))
                                           (reset! text-value new-value)))}])))

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

(defn date-field
  [model field options]
  [:div.form-group
   [:label {:for field} (or (:caption options)
                            (->caption field))]
   [specialized-text-input model field (merge options {:parse-fn parse-date
                                                       :unparse-fn unparse-date})]])

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
     (->> items
          (map (comp #(if (= (get-in % [1 :value]) (get-in @model field))
                        (update-in % [1] assoc :selected true)
                        %)
                     #(select-option % field)))
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
