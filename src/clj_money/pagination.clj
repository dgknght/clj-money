(ns clj-money.pagination
  (:require [clojure.spec :as s]
            [clj-money.url :as url]
            [clj-money.coercion :as coercion]))

(def ^:private menu-coercion-rules
  (coercion/rules :integer [:page] [:per-page] [:total]))

(defn- nav-item
  "Renders a single navigation item"
  [page current-page per-page url]
  [:li {:class (when (= page current-page) "active")}
   [:a {:href (-> url
                  (url/query {:page page :per-page per-page})
                  url/format-url)}
    (str (+ 1 page))]])

(defn nav
  "Renders pagination navigation for the specified options
  
  :page     - the current page index
  :per-page - the number of entries per page
  :total    - the total number of items to be paginated
  :url      - the base url for the navigation links"
  [options]
  [:nav {:aria-label "Page navigation"}
   [:ul.pagination
    (let [{:keys [url
                  page
                  per-page
                  total]} (-> options
                              (coercion/coerce menu-coercion-rules))
          page-count (Math/ceil (/ total per-page))]
      (map #(nav-item % page per-page url) (range page-count)))]])
