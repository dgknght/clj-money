(ns clj-money.dnd)

(defn files
  [event]
  (let [file-list (-> event .-dataTransfer .-files)
        file-count (.-length file-list)]
    (mapv #(.item file-list %) (range file-count))))
