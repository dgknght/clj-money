(ns clj-money.views.accounts.tags
  "An input for the user tags of an account, or of a group of accounts:
  [tags-input model], where model is an atom (or cursor) holding
  :account/user-tags. The tags already used on any account are suggested."
  (:require [clojure.string :as string]
            [reagent.ratom :refer [make-reaction]]
            [dgknght.app-lib.forms :as forms]
            [clj-money.icons :refer [icon]]
            [clj-money.state :refer [accounts]]))

(defn- tag-elem
  [remove-fn]
  (fn [tag]
    (let [tag-name (name tag)]
      ^{:key (str "tag-" tag-name)}
      [:div.d-flex.rounded.bg-secondary.px-2.me-2.mb-2
       [:span tag-name]
       [:a.ms-1.text-decoration-none
        {:href "#"
         :title "Click here to remove this tag"
         :on-click (fn [] (remove-fn tag))}
        (icon :x :size :small)]])))

(defn- tag-elems
  [tags remove-fn]
  [:div.d-flex.flex-wrap.mt-3
   (if (seq tags)
     (->> tags
          (map (tag-elem remove-fn))
          doall)
     [:span.text-muted "None"])])

(defn- apply-tag!
  [model tag]
  ; the tag may arrive as a keyword or a string
  (when (seq (some-> tag name))
    (swap! model
           #(-> %
                (update-in [:account/user-tags]
                           (fnil conj #{})
                           (keyword tag))
                (dissoc ::working-tag)))))

(defn tags-input
  [model]
  (let [all-user-tags (make-reaction #(->> @accounts
                                           (mapcat :account/user-tags)
                                           set))
        apply-tag (partial apply-tag! model)]
    (fn []
      [:<>
       [forms/typeahead-input
        model
        [::working-tag]
        {:mode :direct
         :search-fn (fn [term callback]
                      (let [existing (or (:account/user-tags @model) #{})]
                        (->> @all-user-tags
                             (remove existing)
                             (map name)
                             (filter #(string/includes? % term))
                             callback)))
         :caption-fn name
         :value-fn name
         :find-fn keyword
         :on-blur apply-tag
         :on-change apply-tag}]
       (tag-elems (:account/user-tags @model)
                  #(swap! model update-in [:account/user-tags] disj %))])))
