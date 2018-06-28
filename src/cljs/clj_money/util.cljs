(ns clj-money.util
  (:require [goog.string :as gstring]
            [clojure.string :as string]))

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
    :class - the HTML class(es) to be applied to the anchor element"
  ([caption url] (link-to caption url {}))
  ([caption url options]
   "test"
   [:a {:href url
        :class (:class options)}
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
   [:button {:class (:class options)
             :on-click on-click}
    (when-let [icon (:icon options)]
      [:span.glyphicon {:aria-hidden "true"
                        :class (str "glyphicon-" (if (keyword? icon)
                                                   (name icon)
                                                   (str icon)))}])
    (when (:icon options)
      (space))
    caption]))
