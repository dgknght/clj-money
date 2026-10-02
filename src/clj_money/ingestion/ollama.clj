(ns clj-money.ingestion.ollama
  (:require [clojure.pprint :refer [pprint]]
            [clojure.java.io :as io]
            [cheshire.core :as json]
            [clj-http.client :as http]
            [clj-money.ingestion :as ing])
  (:import java.util.Base64))

(defn- ->base64
  [input]

  (pprint {::->base64 input})

  (let [input (io/input-stream input)]

    (pprint {::input input})

    (.encodeToString (Base64/getEncoder)
                     (.readAllBytes input))))

(defn- read-receipt*
  [source {:keys [model] :or {model  "qwen2.5vl:7b"}}]
  (let [req {:content-type "application/json"
             :accept "application/json"
             :as :json
             :body (json/generate-string
                     {:model model
                      :messages [{:role "user"
                                  :content [{:type "text"
                                             :text "Describe this image."}
                                            {:type "image_url"
                                             :image_url {:url "gt"}}]
                                  }]
                      :images [(->base64 source)]})}
        {:keys [status body]} (http/post "http://localhost:11434/v1/chat/completions"
                                         req)]
    (if (<= 200 status 299)
      body
      (throw (ex-info "Error accessing the ollama service." {:source source})))))

(defmethod ing/reader ::ollama
  [config]
  (reify ing/Reader
    (read-receipt [_ source] (read-receipt* source config))))
