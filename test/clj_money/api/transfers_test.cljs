(ns clj-money.api.transfers-test
  (:require [cljs.test :refer [deftest is]]
            [clj-money.decimal :as d]
            [clj-money.api :as api]
            [clj-money.state :refer [current-entity]]
            [clj-money.api.transfers :as transfers]))

(deftest create-a-transfer
  (let [posted (atom nil)]
    (with-redefs [current-entity (atom {:id 4})
                  api/post (fn [url payload _opts]
                              (reset! posted [url payload]))]
      (transfers/create #:transfer{:date "2026-09-10"
                                   :shares (d/d "10")
                                   :commodity {:id 3 :commodity/symbol "AAPL"}
                                   ; From a commodity account, the trade form's
                                   ; account is a reference to its parent
                                   :from-account {:id 1}
                                   :to-account {:id 2 :account/name "IRA 2"}}))
    (let [[url payload] @posted]
      (is (re-find #"/entities/4/transfers$" url)
          "The transfer is posted for the current entity")
      (is (= #:transfer{:date "2026-09-10"
                        :commodity {:id 3}
                        :from-account {:id 1}
                        :to-account {:id 2}}
             (dissoc payload :transfer/shares))
          "The commodity and accounts are sent as references")
      (is (= "10" (str (:transfer/shares payload)))
          "The shares are sent"))))
