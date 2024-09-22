(ns clj-money.icons)

(defn- apply-size
  [attr {:keys [size]}]
  (merge attr (case (or size :medium)
                :large {:width 40 :height 40}
                :medium-large {:width 32 :height 32}
                :medium {:width 24 :height 24}
                :small {:width 16 :height 16})))

(defn- icon*
  [icon-id opts]
  [:svg.bi (apply-size {:fill "currentColor"} opts)
   [:use {:href (str "/images/bootstrap-icons.svg#"
                     (name icon-id))}]])

(defn icon
  [icon-id & {:as opts}]
  (icon* icon-id opts))

(defn icon-with-text
  [icon-id text & {:as opts}]
  [:span.d-flex.align-items-center
   (icon* icon-id opts)
   [:span.ms-2 text]])
