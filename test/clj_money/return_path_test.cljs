(ns clj-money.return-path-test
  (:require [cljs.test :refer [deftest is]]
            [clj-money.return-path :as rp]))

(defn- fake-storage []
  (let [items (atom {})]
    #js {:getItem    (fn [k] (get @items k))
         :setItem    (fn [k v] (swap! items assoc k v))
         :removeItem (fn [k] (swap! items dissoc k))}))

(deftest the-return-path-is-taken-once
  (let [storage (fake-storage)]
    (rp/remember! "/cli/login?code=BCDF-GHJK" storage)
    (is (= "/cli/login?code=BCDF-GHJK" (rp/take! storage)))
    (is (nil? (rp/take! storage))
        "The path is forgotten once taken")))

(deftest nothing-is-returned-when-nothing-is-remembered
  (is (nil? (rp/take! (fake-storage)))))

(deftest only-a-path-on-this-site-is-remembered
  (doseq [path ["https://example.com/" "//example.com/" "/\\example.com/"]]
    (let [storage (fake-storage)]
      (rp/remember! path storage)
      (is (nil? (rp/take! storage))
          (str "\"" path "\" is not remembered")))))

(deftest only-a-path-on-this-site-is-returned
  (doseq [path ["https://example.com/" "//example.com/" "/\\example.com/"]]
    (let [storage (fake-storage)]
      (.setItem storage "return-path" path)
      (is (nil? (rp/take! storage))
          (str "\"" path "\" is ignored")))))

(deftest unavailable-storage-is-tolerated
  (let [broken #js {:getItem    #(throw (js/Error. "blocked"))
                    :setItem    #(throw (js/Error. "blocked"))
                    :removeItem #(throw (js/Error. "blocked"))}]
    (is (nil? (rp/remember! "/cli/login" broken)))
    (is (nil? (rp/take! broken)))))
