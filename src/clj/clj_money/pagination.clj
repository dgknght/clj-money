(ns clj-money.pagination
  (:require [clojure.spec.alpha :as s]
            [clj-money.url :as url]))

(s/def ::page integer?)
(s/def ::per-page integer?)
(s/def ::total integer?)
(s/def ::url map?)
(s/def ::max-pages (s/and integer? #(< 3)))
(s/def ::pagination-options (s/keys :req-un [::total ::url] :opt-un [::page ::per-page]))

(defn- validate-options
  [options]
  (if (s/valid? ::pagination-options options)
    (merge {:page 1 :per-page 10 :max-pages 10} options)
    (throw (RuntimeException. (str "Invalid pagination options: "
                                   (s/explain-data ::pagination-options options))))))

(defn nav
  "Renders pagination navigation for the specified options

  :page      - the current page index
  :per-page  - the number of entries per page
  :total     - the total number of items to be paginated
  :url       - the base url for the navigation links"
  [options]
  [:nav {:aria-label "Page navigation"}
   (let [{:keys [url total per-page page]} (validate-options options)
         page-count (int (Math/ceil (/ total per-page)))]
     [:nav {:aria-label "Page navigation"}
      [:div.row
       [:div.col-sm-5.text-right
        [:div.btn-group
         [:a.btn.btn-default {:href (-> url
                                        (url/query {:page 1
                                                    :per-page per-page})
                                        url/format-url)}
          [:span.glyphicon.glyphicon-fast-backward {:aria-hidden true}]]
         [:a.btn.btn-default {:href (-> url
                                        (url/query {:page (if (= 1 page)
                                                            1
                                                            (- page 1))
                                                    :per-page per-page})
                                        url/format-url)}
          [:span.glyphicon.glyphicon-step-backward {:aria-hidden true}]]]]
       [:div.col-sm-2.text-center
        [:div.input-group
         [:input#page-input.form-control {:type "text"
                                          :name "page"
                                          :value page}]
         [:span.input-group-btn
          [:button.btn.btn-default
           {:data-url (-> url
                          (url/query {:page "_p_"
                                      :per-page per-page})
                          url/format-url)
            :onclick "window.location.href = this.dataset.url.replace('_p_', $(\"#page-input\").val());"}
           "Go"]]]]
       [:div.col-sm-5.text-left
        [:div.btn-group
         [:a.btn.btn-default {:href (-> url
                                        (url/query {:page (if (= page-count page)
                                                            page-count
                                                            (+ page 1))
                                                    :per-page per-page})
                                        url/format-url)}
          [:span.glyphicon.glyphicon-step-forward {:aria-hidden true}]]
         [:a.btn.btn-default {:href (-> url
                                        (url/query {:page page-count
                                                    :per-page per-page})
                                        url/format-url)}
          [:span.glyphicon.glyphicon-fast-forward {:aria-hidden true}]]]]]])])

(defn prepare-options
  "Accepts a map of parameters (as from a web request) and returns
  the values suitable to be passed to the data layer to
  select the correct section of a list"
  [options]
  (-> options
      (select-keys [:page :per-page])
      (update-in [:page] (fnil #(Integer. %) 1))
      (update-in [:per-page] (fnil #(Integer. %) 10))))
