(ns clj-money.prices.api-client-test
  (:require [clojure.test :refer [deftest is]]
            [clj-time.core :as t]
            [clj-http.client :as client]
            [clj-money.prices.api-client :as prices-client]))

(def ^:private raw-response
  {:request-time 201
   :repeatable? false
   :protocol-version  {:name "HTTP"
                       :major 1
                       :minor 1}
   :streaming? true
   :chunked? false
   :cookies {"3481%5F0"
             {:discard true
              :path "/"
              :secure false
              :value "F8B99CEB9F3EF617384253F9E2BE529C0D4FF51257DBA979309EA21E77694DB7"
              :version 0}
             "GZIP"
             {:discard false
              :expires #inst "2022-05-18T00:59:49.000-00:00"
              :path "/"
              :secure false
              :value "1"
              :version 0}}
   :reason-phrase "OK"
   :headers {"Server" ""
             "Content-Type" "text/javascript; charset=UTF-8"
             "Connection" "close"
             "P3P" "CP=\"NON PHY ONL UNI PUR FIN COM NAV INT DEM STA HEA CUR ADM DEV OUR IND\"policyref=\"/w3c/p3p.xml\""
             "X-AspNet-Version" ""
             "Date" "Fri19 May 2017 00:59:48 GMT"
             "Vary" "Accept-Encoding"
             "X-Powered-By" "ASP.NET"
             "Cache-Control" "private"}
   :orig-content-encoding nil
   :status 200
   :length -1
   :body
   {:MarketCap 795319153600
    :MSDate 42873
    :Low 151.13
    :Volume 33429851
    :Change 2.28999999999999
    :High 153.34
    :ChangeYTD 115.82
    :ChangePercentYTD 31.7043688482127
    :Name "Apple Inc"
    :Timestamp "Thu Mar 2 00:00:00 UTC-04:00 2017"
    :LastPrice 123.45
    :ChangePercent 1.52412645590682
    :Symbol "AAPL"
    :Open 151.27
    :Status "SUCCESS"}
   :trace-redirects []})

(deftest fetch-latest-price
  (with-redefs [client/get (fn [& _]
                             raw-response)]
    (let [expected {:price 123.45M
                    :trade-date (t/local-date 2017 3 2)}
          actual (prices-client/fetch {:symbol "AAPL"})]
      (is (= expected actual) "The correct information is returned"))))
