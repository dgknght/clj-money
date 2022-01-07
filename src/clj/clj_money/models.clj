(ns clj-money.models
  (:require [clojure.spec.alpha :as s]) )

(s/def ::id integer?)
(s/def ::exchange #{:nyse :nasdaq :amex :otc})
