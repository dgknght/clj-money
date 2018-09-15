(ns clj-money.util
  (:require [goog.string :as gstring]
            [clojure.string :as string]
            [cljs-time.format :as f]))

(defn parse-date [str-date]
  (f/parse-local (f/formatters :date) str-date))

(defn parse-date-time [str-date-time]
  (f/parse (f/formatters :date-time-no-ms) str-date-time))

(defn format-date [date]
  (f/unparse-local (f/formatter "M/d/yyyy") date))

(defn format-date-time [date-time]
  (f/unparse (f/formatter "M/d/yyyy h:mm") date-time))

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
