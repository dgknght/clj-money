(ns clj-money.api.cli-logins-test
  (:require [cljs.test :refer [deftest is]]
            [clj-money.api :as api]
            [clj-money.api.cli-logins :as cli-logins]))

(deftest approving-posts-to-the-code's-approve-path
  (let [captured (atom nil)]
    (with-redefs [api/post (fn [path resource _opts]
                             (reset! captured [path resource]))]
      (cli-logins/approve " BCDF-GHJK "))
    (is (= ["/api/cli-logins/BCDF-GHJK/approve" {}] @captured))))
