(ns clj-money.forms
  (:require [clj-money.inflection :as infl]))

(defn- label
  [field]
  [:label.control-label {:for field} (-> field name infl/title-case)])

(defn text-input
  [field & validations]
  [:div.form-group
   (label field)
   [:input.form-control {:field :text :id field}]
   (map #(% field) validations)])

(defn select-input
  [field options]
  [:div.form-group
   (label field)
   "Select coming soon"
   #_[:select.form-control {:field :list :id field}
    (for [option options]
      ^{:key option}
      [:option {:key option} option])]])

(defn required
  [field]
  [:span.help-block {:field :alert :id field :event empty?}
     (str (-> field name infl/title-case) " is required.")])
