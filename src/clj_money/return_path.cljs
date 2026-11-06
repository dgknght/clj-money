(ns clj-money.return-path
  "Where to send the user once they sign in, when they were sent to the sign
  in page from another one, like the page that approves a command-line
  sign-in. Signing in with an OAuth provider reloads the app, so the path is
  kept in the tab's session storage.")

(def ^:private storage-key "return-path")

(defn- session-storage []
  (when (exists? js/sessionStorage)
    js/sessionStorage))

(defn- local-path?
  "True for a path on this site, but not one like //example.com, which a
  browser reads as another site."
  [path]
  (boolean (and (string? path)
                (re-find #"^/(?![/\\])" path))))

(defn remember!
  "Remembers the path to come back to once the user signs in."
  ([path] (remember! path (session-storage)))
  ([path storage]
   (when (local-path? path)
     (try
       (.setItem storage storage-key path)
       (catch :default _ nil)))
   nil))

(defn take!
  "Returns, and forgets, the remembered path, if any."
  ([] (take! (session-storage)))
  ([storage]
   (try
     (let [path (.getItem storage storage-key)]
       (.removeItem storage storage-key)
       (when (local-path? path) path))
     (catch :default _ nil))))
