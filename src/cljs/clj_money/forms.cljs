(ns clj-money.forms
  (:require-macros [reagent-forms.macros :refer [render-element]])
  (:require
            [reagent.core :as r]
            [reagent-forms.core :refer [value-of
                                        default-selection]]
            [clj-money.inflection :as infl]))

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
   [:div (merge options {:field :typeahead
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
  [[tag {:keys [list-fn id] :as attr}] {:keys [doc save! get]}]
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
