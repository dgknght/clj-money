(ns clj-money.base64
  (:import java.util.Base64))

(defn encode
  "Returns the bytes base64-encoded"
  [^bytes content]
  (.encodeToString (Base64/getEncoder) content))
