(ns clj-money.util
  (:require [goog.string :as gstring]
            [clojure.string :as string]
            [cljs-time.format :as f]
            [clj-money.macros :refer-macros [with-retry]])
  (:import goog.i18n.NumberFormat))

(defn parse-date [str-date]
  (f/parse-local (f/formatters :date) str-date))

(defn parse-date-time [str-date-time]
  (f/parse (f/formatters :date-time-no-ms) str-date-time))

(defn format-date [date]
  (f/unparse-local-date (f/formatter "M/d/yyyy") date))

(defn format-date-time [date-time]
  (f/unparse (f/formatter "M/d/yyyy h:mm") date-time))

(defn format-percent
  ([value]
   (format-percent value 1))
  ([value places]
   (when value
     (gstring/format (str "%." places "f%") (* 100 value)))))

(defn format-decimal
  ([value] (format-decimal value 2))
  ([value places]
   (let [number-format (doto (NumberFormat. (.-DECIMAL (.-Format NumberFormat)))
                         (.setMaximumFractionDigits places)
                         (.setMinimumFractionDigits places))]
     (.format
       number-format
       value))))

(defn space
  "Renders an HTML non-breakable space."
  []
  [:span {:dangerouslySetInnerHTML {:__html "&nbsp;"}}])

(defn- stringify
  [value]
  (if (keyword? value)
    (name value)
    (str value)))

(defn path
  [& segments]
  (str "/" (->> segments
                (map stringify)
                (string/join "/"))))

(defn link-to
  "return hiccup representing an anchor tag. Options can include:
    :icon - a Bootstrap glyphicon. Specify the part after 'glyphicon-'
    :class - the HTML class(es) to be applied to the anchor element
    <other> - any other HTML attributes"
  ([caption url] (link-to caption url {}))
  ([caption url options]
   "test"
   [:a (-> options
           (dissoc :icon)
           (assoc :href url))
    (when-let [icon (:icon options)]
      [:span.glyphicon
        {:aria-hidden true
         :class (str "glyphicon-" (if (keyword? icon)
                                    (name icon)
                                    (str icon)))}])
    (when (:icon options)
      (space))
    caption]))

(defn add-button
  "Renders a link for adding an resource"
  [url]
  (link-to "Add" url {:class "btn btn-primary"
                      :icon :plus}))

(defn button
  ([caption on-click] (button caption on-click {}))
  ([caption on-click options]
   [:button (assoc options :on-click on-click)
    (when-let [icon (:icon options)]
      [:span.glyphicon {:aria-hidden "true"
                        :class (str "glyphicon-" (if (keyword? icon)
                                                   (name icon)
                                                   (str icon)))}])
    (when (:icon options)
      (space))
    caption]))

(defn log->
  "Writes the specified information to the log and returns
  the information as is. Intended for use with left-threading
  functions"
  [info message]
  (.log js/console message (prn-str info))
  info)

(defn log->>
  "Writes the specified information to the log and returns
  the information as is. Intended for use with right-threading
  functions"
  [message info]
  (log-> info message))

(defn set-focus
  [id]
  (with-retry
    (.focus (.getElementById js/document id))))

(defn google-g
  ([] (google-g {}))
  ([options]
   [:svg {:version "1.1"
          :xmlns "http://www.w3.org/2000/svg"
          :width (get-in options [:width] "24px")
          :height (get-in options [:height] "24px")
          :viewBox "0 0 48 48"}
    [:g
     [:path {:fill "#EA4335" :d "M24 9.5c3.54 0 6.71 1.22 9.21 3.6l6.85-6.85C35.9 2.38 30.47 0 24 0 14.62 0 6.51 5.38 2.56 13.22l7.98 6.19C12.43 13.72 17.74 9.5 24 9.5z"}]
     [:path {:fill "#4285F4" :d "M46.98 24.55c0-1.57-.15-3.09-.38-4.55H24v9.02h12.94c-.58 2.96-2.26 5.48-4.78 7.18l7.73 6c4.51-4.18 7.09-10.36 7.09-17.65z"}]
     [:path {:fill "#FBBC05" :d "M10.53 28.59c-.48-1.45-.76-2.99-.76-4.59s.27-3.14.76-4.59l-7.98-6.19C.92 16.46 0 20.12 0 24c0 3.88.92 7.54 2.56 10.78l7.97-6.19z"}]
     [:path {:fill "#34A853" :d "M24 48c6.48 0 11.93-2.13 15.89-5.81l-7.73-6c-2.15 1.45-4.92 2.3-8.16 2.3-6.26 0-11.57-4.22-13.47-9.91l-7.98 6.19C6.51 42.62 14.62 48 24 48z"}]
     [:path {:fill "none"    :d "M0 0h48v48H0z"}]]]))

(defn google-signin-link
  ([] (google-signin-link {}))
  ([options]
   [:a.btn.btn-light {:href "/auth/google/start"}
    (google-g options)
    " Sign In With Google"]))
