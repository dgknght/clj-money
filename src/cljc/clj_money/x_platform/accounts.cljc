(ns clj-money.x-platform.accounts)

(def account-types
  "The list of valid account types in standard presentation order"
  [:asset :liability :equity :income :expense])

(defn- append-path
  [account parent]
  (assoc account :path (str (:path parent) "/" (:name account))))

(defn- append-children
  [account all-accounts]
  (let [children (->> all-accounts
                      (filter #(= (:id account) (:parent-id %)))
                      (map #(append-path % account))
                      (map #(append-children % all-accounts))
                      (sort-by :name)
                      vec)]
    (assoc account :children children
                   :children-value (reduce #(+ %1 (:value %2) (:children-value %2))
                                             0
                                             children))))

(defn nest
  "Accepts a list of accounts and nests
  children under parents"
  ([accounts]
   (nest account-types accounts))
  ([types accounts]
   (let [grouped (->> accounts
                      (remove :parent-id)
                      (map #(assoc % :path (:name %)))
                      (map #(append-children % accounts))
                      (group-by :type))]
     (mapv #(hash-map :type % :accounts (or
                                          (->> grouped
                                               %
                                               (sort-by :name)
                                               vec)
                                          []))
           types))))

(defn- unnest*
  [accumulator accounts]
  (reduce (fn [r account]
            (unnest* (conj r (dissoc account :children))
                     (:children account)))
          accumulator
          accounts))

(defn unnest
  "Accepts a nested set of accounts and flattens the heirarchy,
  retaining the :path attribute"
  [accounts]
  (->> accounts
       (mapcat :accounts)
       (unnest* [])))
