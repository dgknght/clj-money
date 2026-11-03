(ns clj-money.crypto
  "Encrypts secrets the application has to keep in a form it can read back,
  like the password of a mailbox it checks for receipts, with AES-GCM. The
  key is 32 bytes, configured base64-encoded (e.g., generated with
  `openssl rand -base64 32`)."
  (:import java.security.SecureRandom
           java.util.Base64
           javax.crypto.Cipher
           [javax.crypto.spec GCMParameterSpec SecretKeySpec]))

(def ^:private iv-length 12)
(def ^:private tag-bits 128)

(defn- ->key
  [encoded-key]
  (when-not encoded-key
    (throw (ex-info "No encryption key is configured" {})))
  (let [key-bytes (.decode (Base64/getDecoder) ^String encoded-key)]
    (when-not (= 32 (alength key-bytes))
      (throw (ex-info "The encryption key must be 32 bytes, base64-encoded"
                      {:length (alength key-bytes)})))
    (SecretKeySpec. key-bytes "AES")))

(defn- cipher
  [mode encoded-key ^bytes iv]
  (doto (Cipher/getInstance "AES/GCM/NoPadding")
    (.init (int mode)
           (->key encoded-key)
           (GCMParameterSpec. tag-bits iv))))

(defn encrypt
  "Returns the text encrypted with the key, base64-encoded, with the random
  IV it was encrypted with in front of it"
  [^String text encoded-key]
  (let [iv (byte-array iv-length)
        _ (.nextBytes (SecureRandom.) iv)
        encrypted (.doFinal (cipher Cipher/ENCRYPT_MODE encoded-key iv)
                            (.getBytes text "UTF-8"))
        result (byte-array (+ iv-length (alength encrypted)))]
    (System/arraycopy iv 0 result 0 iv-length)
    (System/arraycopy encrypted 0 result iv-length (alength encrypted))
    (.encodeToString (Base64/getEncoder) result)))

(defn decrypt
  "Returns the text that encrypt encrypted with the key. Throws if the key
  is not the one it was encrypted with, or the value was altered."
  [^String encrypted encoded-key]
  (let [data (.decode (Base64/getDecoder) encrypted)
        iv (java.util.Arrays/copyOfRange data 0 (int iv-length))
        body (java.util.Arrays/copyOfRange data (int iv-length) (alength data))]
    (String. (.doFinal (cipher Cipher/DECRYPT_MODE encoded-key iv) body)
             "UTF-8")))
