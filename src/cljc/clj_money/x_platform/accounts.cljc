(ns clj-money.x-platform.accounts)

(def account-types
  "The list of valid account types in standard presentation order"
  [:asset :liability :equity :income :expense])

(defn- append-path
  [account parent]
  (assoc account
         :path (str (:path parent) "/" (:name account))
         :parents ((fnil conj []) (:parents parent) (:id parent))))

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
            (unnest* (conj r (-> account
                                 (assoc :has-children?
                                        (boolean (seq (:children account))))
                                 (dissoc account :children)))
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

(defn left-side?
  "Returns truthy if the specified account is asset or expense, falsey if anything else"
  [account]
  (#{:asset :expense} (:type account)))

(def right-side? (comp left-side?))

(defn- polarizer
  [transaction-item account]
  (* (if (left-side? account) 1 -1)
     (if (= :debit (:action transaction-item)) 1 -1)))

(defn polarize-item
  [transaction-item account]
  (let [polarizer (polarizer transaction-item account)]
    (assoc transaction-item
           :polarized-quantity (* polarizer (:quantity transaction-item))
           :polarized-value    (* polarizer (:value transaction-item)))))

(defn polarize-quantity
  "Adjusts the polarity of a quantity as appropriate given
  a transaction item action and the type of the associated account"
  [transaction-item account]
  (* (:quantity transaction-item account)
     (polarizer transaction-item account)))

(defn derive-action
  "Given a quantity (either positve or negative) and an
  account, returns the appropriate action (debit or credit)"
  [quantity account]
  (if (< quantity 0)
    (if (left-side? account)
      :credit
      :debit)
    (if (left-side? account)
      :debit
      :credit)))

(defn- abs
  [value]
  #?(:clj (.abs value) ; we're assuming BigDecimal here
     :cljs (Math/abs value)))

(defn derive-item
  "Given a quantity and an account, returns a transaction item
  with appropriate attributes"
  [quantity account]
  {:quantity (abs quantity)
   :account-id (:id account)
   :action (derive-action quantity account)})
