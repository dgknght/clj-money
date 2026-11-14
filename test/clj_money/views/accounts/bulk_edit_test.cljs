(ns clj-money.views.accounts.bulk-edit-test
  (:require [cljs.test :refer [deftest is async]]
            [cljs.core.async :as a]
            [cljs-http.client :as http]
            [dgknght.app-lib.notifications :as notify]
            [clj-money.state :refer [app-state]]
            [clj-money.views.accounts.bulk-edit :as bulk-edit]))

(defn- respond-with
  "Returns a stand-in for cljs-http's patch that answers every request
  with the response for the account in the given map and, like cljs-http,
  closes the channel after the response."
  [responses]
  (fn [uri {:keys [channel]}]
    (a/put! channel (some (fn [[id res]]
                            (when (re-find (re-pattern (str id "$")) uri)
                              res))
                          responses))
    (a/close! channel)
    channel))

(defn- save-with
  "Submits the bulk edit form for the given accounts with the PATCH
  requests stubbed to return the given responses, calling done with what
  was captured once the form reports that the save is finished. The stubs
  are set, rather than redefined, because the save continues after
  with-redefs would have restored them."
  [responses done]
  (let [captured (atom {:warnings []
                        :toasts []})
        originals [http/patch notify/warnf notify/toast notify/dangerf]
        accounts (:accounts @app-state)
        restore (fn []
                  (swap! app-state assoc :accounts accounts)
                  (let [[p w t d] originals]
                    (set! http/patch p)
                    (set! notify/warnf w)
                    (set! notify/toast t)
                    (set! notify/dangerf d)))
        bulk (atom {:account-ids (keys responses)
                    :account/user-tags #{:mandatory}})
        [_ {:keys [on-submit]}] (bulk-edit/bulk-edit-form
                                  bulk
                                  :on-save #(js/setTimeout
                                              (fn []
                                                (restore)
                                                (done @captured))
                                              10)
                                  :on-cancel identity)]
    (swap! app-state assoc :accounts (mapv (fn [id]
                                             {:id id
                                              :account/name (str "Account " id)
                                              :account/user-tags #{}})
                                           (keys responses)))
    (set! http/patch (respond-with responses))
    (set! notify/warnf (fn [msg & args]
                         (swap! captured update :warnings conj (apply str msg args))))
    (set! notify/toast (fn [title msg]
                         (swap! captured update :toasts conj [title msg])))
    (set! notify/dangerf (fn [& _]))
    (on-submit #js {:preventDefault identity})))

(deftest a-failed-account-update-is-reported
  (async done
         (save-with {101 {:status 200
                          :body {:id 101
                                 :account/user-tags [:mandatory]}}
                     102 {:status 500
                          :body {:message "Something went wrong"}}}
                    (fn [{:keys [warnings toasts]}]
                      (is (= [["Update Finished" "Updated 1 account(s)."]]
                             toasts)
                          "The successful update is counted")
                      (is (= 1 (count warnings))
                          "The failed update is reported")
                      (done)))))
