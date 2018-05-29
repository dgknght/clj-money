(ns clj-money.forms
  (:require [clj-money.inflection :as infl]))

(defn- label
  [field]
  [:label.control-label {:for field} (-> field name infl/last-segment infl/title-case)])

(defn text-input
  [field & validations]
  [:div.form-group
   (label field)
   [:input.form-control {:field :text :id field}]
   (for [validation validations]
     (validation field))])

(defn select-input
  [field options]
  [:div.form-group
   (label field)
   [:select.form-control {:field :list :id field}
    (for [option options]
      ^{:key (str (name field) ".option-" option)}
      [:option {:key option} option])]])

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

(defn required
  [field]
  ^{:key (str (name field) ".required")}
  [:span.help-block {:field :alert :id field :event empty?}
   (str (-> field name infl/last-segment infl/title-case) " is required.")])
