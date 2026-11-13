(ns clj-money.commands
  "The registry and matcher behind the command palette (issue #309). A
  command is a map with a :label, the :path it navigates to and, optionally,
  :keywords that match it as well as the label."
  (:require [clojure.string :as string]))

(defn- encode
  [s]
  #?(:clj (string/replace (java.net.URLEncoder/encode s "UTF-8") "+" "%20")
     :cljs (js/encodeURIComponent s)))

(defn nav-commands
  "Commands that go to the pages of the nav items"
  [items]
  (map (fn [{:keys [id path label infered-label]}]
         {:id [:nav id]
          :label (str "Go to " (or label infered-label))
          :keywords [(or label infered-label)]
          :path path})
       items))

(defn account-commands
  "Commands that act on each of the accounts, matched by the account's
  name or its full path, e.g. \"Checking\" or \"Assets/Checking\""
  [accounts]
  (mapcat (fn [{:keys [id] :account/keys [path hidden]}]
            (when-not hidden
              (let [full-name (string/join "/" path)
                    keywords [(last path) full-name]
                    account-path (str "/accounts/" (encode (str id)))]
                [{:id [:account-transactions id]
                  :label (str "View transactions for " full-name)
                  :keywords keywords
                  :path account-path}
                 {:id [:edit-account id]
                  :label (str "Edit account " full-name)
                  :keywords keywords
                  :path (str account-path "/edit")}])))
          accounts))

(defn- search-command
  [input]
  {:id [:search input]
   :label (str "Search transactions for '" input "'")
   :path (str "/search?date-range=this-year&description=" (encode input))})

(defn- separator?
  "True if the character separates words, e.g., the / in an account's path.
  Not [^\\p{L}\\p{N}], which a JavaScript regex reads as literal characters."
  [c]
  (re-matches #"[\s/:._,&()\-]" c))

(defn- word-start?
  "True if term appears in text at the start of a word other than the first"
  [text term]
  (loop [from 1]
    (when-let [i (string/index-of text term from)]
      (or (separator? (subs text (dec i) i))
          (recur (inc i))))))

(defn- rank
  "Ranks how well the term matches the text: 0 if the text starts with
  it, 1 if a later word does and 2 if it appears anywhere else, or nil if
  it doesn't match"
  [text term]
  (let [text (string/lower-case text)]
    (cond
      (string/starts-with? text term) 0
      (word-start? text term) 1
      (string/includes? text term) 2)))

(defn- command-rank
  [{:keys [label keywords]} term]
  (some->> (cons label keywords)
           (keep #(rank % term))
           seq
           (apply min)))

(defn search
  "Returns the commands that match the input, best matches first: those
  whose label or a keyword starts with the input, then those with a word
  that starts with it, then those that contain it. Commands that match
  equally well keep their order. Unless the input is blank, the last
  command searches the transactions for it."
  [commands input]
  (let [input (string/trim (or input ""))
        term (string/lower-case input)]
    (when-not (string/blank? input)
      (conj (->> commands
                 (keep #(when-let [r (command-rank % term)]
                          [r %]))
                 (sort-by first)
                 (mapv second))
            (search-command input)))))
