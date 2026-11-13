(ns clj-money.nav
  "Organizes the nav items (issue #310). The items stay a flat list for
  the bottom nav and the command palette, but an item can name a :group,
  which the desktop navbar shows as a dropdown and the bottom nav's More
  sheet as a heading.")

(def groups
  [{:id :activity
    :label "Activity"
    :tool-tip "Enter, reconcile and review transactions"}
   {:id :setup
    :label "Setup"
    :tool-tip "Manage commodities, imports, entities and users"}])

(def ^:private group-map
  (into {} (map (juxt :id identity)) groups))

(defn- group-item
  [group-id children]
  (let [badged (filter :badge children)]
    (cond-> (assoc (group-map group-id)
                   :children children
                   :active? (boolean (some :active? children)))
      (seq badged)
      (assoc :badge (reduce + (map :badge badged))
             :badge-class (:badge-class (first badged))))))

(defn navbar-items
  "Returns the items for the desktop navbar: the ungrouped items, and a
  dropdown item in place of each group, where its first member would be.
  Search is left out, as the navbar shows it as a button that opens the
  command palette."
  [items]
  (let [items (remove #(= :search (:id %)) items)
        members (group-by :group items)]
    (->> items
         (keep (fn [{:keys [group] :as item}]
                 (if group
                   (when (= item (first (members group)))
                     (group-item group (members group)))
                   item)))
         vec)))

(defn sections
  "Returns the items as [label items] pairs, the ungrouped items first
  with a nil label, followed by each group in order"
  [items]
  (let [members (group-by :group items)]
    (->> (cons [nil (members nil)]
               (map (fn [{:keys [id label]}]
                      [label (members id)])
                    groups))
         (filter (comp seq second)))))
