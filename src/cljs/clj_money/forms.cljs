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
      ^{:key option}
      [:option {:key option} option])]])

(defn required
  [field]
  ^{:key (keyword (str (name field) "-required"))}
  [:span.help-block {:field :alert :id field :event empty?}
   (str (-> field name infl/last-segment infl/title-case) " is required.")])
