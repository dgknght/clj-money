(ns clj-money.pagination
  (:require [clojure.spec :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.set :refer [rename-keys]]
            [clj-money.url :as url]
            [clj-money.coercion :as coercion]))

(s/def ::page integer?)
(s/def ::per-page integer?)
(s/def ::total integer?)
(s/def ::url map?)
(s/def ::max-pages (s/and integer? #(< 3)))
(s/def ::pagination-options (s/keys :req-un [::total ::url] :opt-un [::page ::per-page]))

(def ^:private menu-coercion-rules
  (coercion/rules :integer [:page] [:per-page] [:total] [:max-pages]))

(defn- validate-options
  [options]
  (let [coerced (coercion/coerce menu-coercion-rules options)]
    (if (s/valid? ::pagination-options coerced)
      (merge {:page 0 :per-page 10 :max-pages 10} coerced)
      (throw (RuntimeException. (str "Invalid pagination options: "
                                     (s/explain-data ::pagination-options coerced)))))))

(defmulti ^:private nav-item
  "Renders a single navigation item"
  (fn [options]
    (if (vector? (:page options))
      :placeholder
      :regular)))

(defmethod ^:private nav-item :regular
  [{:keys [page current-page per-page url] :as options}]
  [:li {:class (when (= page current-page) "active")}
   [:a {:href (-> url
                  (url/query {:page page :per-page per-page})
                  url/format-url)}
    (str (+ 1 page))]])

(defmethod ^:private nav-item :placeholder
  [{:keys [page current-page per-page url]}]
  [:li
   [:a {:href "#"}
    "..."]])

(defn nav
  "Renders pagination navigation for the specified options
  
  :page      - the current page index
  :per-page  - the number of entries per page
  :total     - the total number of items to be paginated
  :max-pages - the maximum number of pages explicitly listed
  :url       - the base url for the navigation links"
  [options]
  [:nav {:aria-label "Page navigation"}
   [:ul.pagination
    (let [{:keys [total per-page max-pages]
           :as validated-options} (-> options
                                      validate-options
                                      (rename-keys {:page :current-page}))
          page-count (int (Math/ceil (/ total per-page)))
          pages (loop [page 0 result []]
                  (if (<= page-count page)
                    result
                    (recur (if (< max-pages page)
                             page-count
                             (+ page 1))
                           (conj result
                                 (assoc validated-options
                                        :page (if (< max-pages page)
                                                (- page-count 1)
                                                page))))))]
      (map #(nav-item %) pages))]])

(defn prepare-options
  "Accepts a map of parameters (as from a web request) and returns
  the values suitable to be passed to the data layer to
  select the correct section of a list"
  [options]
  (-> options
      (select-keys [:page :per-page])
      (update-in [:page] (fnil #(Integer. %) 0))
      (update-in [:per-page] (fnil #(Integer. %) 10))))
