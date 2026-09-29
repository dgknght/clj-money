(ns clj-money.views.attachments-test
  (:require [cljs.test :refer [deftest is]]
            [clj-money.views.attachments :as atts]))

(deftest remove-an-attachment-from-the-page-state
  (is (= {:attachments [{:id 101 :attachment/caption "receipt"}]}
         (update (atts/remove-attachment
                   {:attachments [{:id 101 :attachment/caption "receipt"}
                                  {:id 102 :attachment/caption "confirmation"}]}
                   {:id 102
                    :attachment/transaction {:id 201}})
                 :attachments
                 vec))
      "The attachment with the matching id is removed from the list"))

(deftest keep-the-attachments-card-open-while-attachments-remain
  (is (= {:id 201}
         (:attachments-item
           (atts/remove-attachment
             {:attachments-item {:id 201}
              :attachments [{:id 101} {:id 102}]}
             {:id 102})))
      "The attachments item is retained"))

(deftest close-the-attachments-card-when-the-last-attachment-is-removed
  (let [result (atts/remove-attachment
                 {:attachments-item {:id 201}
                  :attachments [{:id 101}]}
                 {:id 101})]
    (is (not (contains? result :attachments-item))
        "The attachments item is removed, closing the card")
    (is (empty? (:attachments result))
        "The attachments list is empty")))
