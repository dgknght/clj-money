(ns clj-money.entities)

(defn- entity-row
  [entity]
  [:tr
   [:td (:name entity)]])

; Expects an atom containing a list of entities
(defn management
  "Renders an entity management form"
  [entities]
  [:section
   [:h1 "Entities"]
   [:table.table.table-striped.table-hover
    [:tr
     [:th "Name"]]
    (doall (map entity-row @entities))]])
