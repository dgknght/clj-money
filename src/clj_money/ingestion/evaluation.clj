(ns clj-money.ingestion.evaluation
  "Evaluates vision models at reading receipts. Each model is run, with and
  without the GPU, against a set of receipt images that have hand-written
  expected answers, and each answer is scored for accuracy and speed.

  Run with `lein eval-receipts -- --help`."
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.data.csv :as csv]
            [java-time.api :as t]
            [cheshire.core :as json]
            [clj-http.client :as http]
            [camel-snake-kebab.core :refer [->kebab-case-keyword]]
            [clj-money.cli :refer [with-options
                                   default-options]]
            [clj-money.ingestion.receipts :as rcpts]
            [clj-money.ingestion.ollama :as ollama])
  (:import [java.time LocalDate]
           [java.time.format DateTimeFormatter]))

; Scoring ----------------------------------------------------------------------

(defn- ->dec
  [v]
  (cond
    (decimal? v) v
    (number? v) (bigdec v)
    (string? v) (try (bigdec (str/replace v #"[^0-9.\-]" ""))
                     (catch NumberFormatException _ nil))
    :else nil))

(defn- amount=
  "True if the two amounts are equal to the cent."
  [expected actual]
  (when-let [a (->dec actual)]
    (< (abs (- (->dec expected) a)) 0.005M)))

(defn- tax-rate=
  "True if the two tax rates are equal. A rate given as a percentage (e.g.,
  8.25) is treated as the equivalent fraction (0.0825)."
  [expected actual]
  (when-let [a (->dec actual)]
    (let [a (if (< 1M a) (/ a 100M) a)]
      (< (abs (- (->dec expected) a)) 0.0005M))))

(defn- tokens
  [s]
  (->> (str/split (str/lower-case (str s)) #"[^a-z0-9]+")
       (remove str/blank?)
       (map #(get {"street" "st"
                   "road" "rd"
                   "drive" "dr"
                   "suite" "ste"
                   "texas" "tx"}
                  %
                  %))
       set))

(defn similarity
  "Returns the share of words the two strings have in common, from 0 to 1."
  [a b]
  (let [ta (tokens a)
        tb (tokens b)]
    (if (and (seq ta) (seq tb))
      (/ (count (filter ta tb))
         (double (count (into ta tb))))
      0.0)))

(defn- squash
  [s]
  (str/replace (str/lower-case (str s)) #"[^a-z0-9]" ""))

(defn name=
  "True if the merchant names match, allowing for one to be a longer form of
  the other (e.g., \"CAVA\" and \"CAVA Plano\")."
  [expected actual]
  (let [e (squash expected)
        a (squash actual)]
    (boolean
      (and (seq a)
           (or (str/includes? a e)
               (str/includes? e a)
               (<= 0.5 (similarity expected actual)))))))

(def ^:private date-formats
  (mapv #(DateTimeFormatter/ofPattern %)
        ["yyyy-MM-dd" "M/d/yyyy" "M/d/yy" "M-d-yyyy" "M-d-yy" "yyyy/M/d"]))

(defn parse-date
  "Parses a date written in any of the common receipt formats, returning nil
  if it can't be parsed."
  [s]
  (when (string? s)
    (let [s (str/trim (if (re-find #"^\d{4}-\d{2}-\d{2}T" s)
                        (subs s 0 10)
                        s))]
      (some #(try (LocalDate/parse s %)
                  (catch Exception _ nil))
            date-formats))))

(defn- present?
  "True if the model supplied a value, as opposed to leaving it null or
  writing a placeholder."
  [v]
  (not (or (nil? v)
           (and (string? v)
                (contains? #{"" "unknown" "n/a" "none" "null"}
                           (str/lower-case (str/trim v)))))))

(defn- account=
  [expected actual]
  (if (set? expected)
    (contains? expected actual)
    (= expected actual)))

(defn- non-zero-items
  [items]
  (remove #(let [a (->dec (:amount %))]
             (or (nil? a) (zero? a)))
          items))

(defn match-items
  "Pairs each expected line item with a predicted item of the same amount,
  preferring the one with the most similar description. Items with no
  amount (e.g., toppings listed at $0.00) are ignored on both sides."
  [expected predicted]
  (loop [[e & es] (non-zero-items expected)
         remaining (vec (non-zero-items predicted))
         pairs []]
    (if-not e
      {:pairs pairs
       :unmatched-expected (- (count (non-zero-items expected)) (count pairs))
       :unmatched-predicted (count remaining)}
      (let [candidates (keep-indexed (fn [i p]
                                       (when (amount= (:amount e) (:amount p))
                                         [i p]))
                                     remaining)]
        (if (seq candidates)
          (let [[i p] (apply max-key
                             #(similarity (:description e)
                                          (:description (second %)))
                             candidates)]
            (recur es
                   (into (subvec remaining 0 i) (subvec remaining (inc i)))
                   (conj pairs [e p])))
          (recur es remaining pairs))))))

(defn- ratio
  [n d]
  (when (pos? d) (/ n (double d))))

(defn score-items
  [expected predicted]
  (let [{:keys [pairs]} (match-items expected predicted)
        n-expected (count (non-zero-items expected))
        n-predicted (count (non-zero-items predicted))
        matched (count pairs)
        precision (or (ratio matched n-predicted)
                      (if (zero? n-expected) 1.0 0.0))
        recall (or (ratio matched n-expected)
                   (if (zero? n-predicted) 1.0 0.0))
        taxable-pairs (filter #(some? (:taxable (first %))) pairs)]
    {:item-precision precision
     :item-recall recall
     :item-f1 (if (zero? (+ precision recall))
                0.0
                (/ (* 2 precision recall) (+ precision recall)))
     :account-accuracy (ratio (count (filter (fn [[e p]]
                                               (account= (:account e)
                                                         (:account p)))
                                             pairs))
                              n-expected)
     :taxable-accuracy (ratio (count (filter (fn [[e p]]
                                               (= (:taxable e)
                                                  (boolean (:taxable p))))
                                             taxable-pairs))
                              (count taxable-pairs))}))

(defn consistent?
  "True if the line items, tax, and tip add up to the total. This needs no
  expected answer, so it can also be checked in production."
  [{:keys [total tax tip line-items]}]
  (when (and (seq line-items) (->dec total))
    (let [sum (->> line-items
                   (map (comp #(or % 0M) ->dec :amount))
                   (reduce + (+ (or (->dec tax) 0M)
                                (or (->dec tip) 0M))))]
      (< (abs (- (->dec total) sum)) 0.02M))))

(def ^:private field-checks
  {:location-name name=
   :location-address (fn [e a] (<= 0.6 (similarity e a)))
   :date (fn [e a] (= (parse-date e) (parse-date a)))
   :total amount=
   :tax amount=
   :tax-rate tax-rate=
   :tip amount=
   :payment-account =})

(def weights
  "The weight each measure carries in the overall score. Measures that don't
  apply to a receipt (e.g., payment account when the receipt doesn't show the
  card) are left out and the rest are scaled up."
  {:total 0.25
   :date 0.15
   :location-name 0.10
   :payment-account 0.10
   :tax 0.10
   :item-f1 0.20
   :account-accuracy 0.10})

(defn- weighted-score
  [measures]
  (let [applicable (filter (comp some? measures key) weights)
        total-weight (reduce + (map val applicable))]
    (when (pos? total-weight)
      (/ (reduce + (map (fn [[k w]]
                          (* w (let [v (measures k)]
                                 (cond (true? v) 1.0
                                       (false? v) 0.0
                                       :else v))))
                        applicable))
         total-weight))))

(defn score
  "Scores a parsed model response against the expected answer.

  Field measures are true or false, or nil when the receipt doesn't show the
  value (in which case supplying one counts as a hallucination instead)."
  [expected actual]
  (let [fields (into {}
                     (map (fn [[k f]]
                            (let [e (get expected k)]
                              [k (when (some? e)
                                   (boolean (f e (get actual k))))])))
                     field-checks)
        hallucinated (->> (keys field-checks)
                          (filter #(and (nil? (get expected %))
                                        (present? (get actual %))))
                          sort
                          vec)
        items (score-items (:line-items expected)
                           (:line-items actual))
        measures (merge fields items)]
    (merge measures
           {:hallucinated hallucinated
            :consistent (consistent? actual)
            :usable (boolean
                      (and (:location-name fields)
                           (:date fields)
                           (:total fields)
                           (not (false? (:payment-account fields)))
                           (= 1.0 (:item-f1 items))))
            :score (weighted-score measures)})))

; Running ----------------------------------------------------------------------

(defn- expand-home
  [path]
  (str/replace-first path #"^~" (System/getProperty "user.home")))

(defn- base-name
  [file]
  (str/replace (.getName (io/file file)) #"\.[^.]+$" ""))

(defn- read-edn
  [file]
  (edn/read-string {:readers {}} (slurp file)))

(defn- load-expected
  [dir]
  (->> (.listFiles (io/file dir))
       (filter #(str/ends-with? (.getName %) ".edn"))
       (map (juxt base-name read-edn))
       (into {})))

(defn- find-receipts
  "Returns the receipt images in the given directories that have an expected
  answer, as maps of :name, :image, and :expected."
  [dirs expected]
  (->> dirs
       (mapcat #(.listFiles (io/file %)))
       (filter #(re-find #"(?i)\.(jpe?g|png)$" (.getName %)))
       (keep (fn [f]
               (if-let [e (expected (base-name f))]
                 {:name (base-name f)
                  :image f
                  :expected e}
                 (binding [*out* *err*]
                   (println "Skipping" (str f) "(no expected answer)")))))
       (sort-by :name)))

(defn- vision-models
  "Returns the installed models that can read images, as maps of :name and
  :capabilities. The tag list also includes runner aliases (e.g.,
  \"llamacpp:<digest>\") for models listed under their own names; those are
  left out."
  [opts]
  (let [{:keys [host port] :or {host "localhost" port 11434}} opts]
    (->> (http/get (format "http://%s:%s/api/tags" host port)
                   {:as :json})
         :body
         :models
         (filter #(some #{"vision"} (:capabilities %)))
         (remove #(str/starts-with? (:name %) "llamacpp:"))
         (map #(select-keys % [:name :capabilities]))
         distinct
         (sort-by :name))))

(defn- model-capabilities
  [model opts]
  (let [{:keys [host port] :or {host "localhost" port 11434}} opts]
    (-> (http/post (format "http://%s:%s/api/show" host port)
                   {:as :json
                    :content-type :json
                    :body (json/generate-string {:model model})})
        :body
        :capabilities)))

(def ^:private hardware-options
  {"gpu" {}
   "cpu" {:num-gpu 0}})

(defn- safe-name
  [s]
  (str/replace s #"[^A-Za-z0-9._-]" "_"))

(defn- seconds
  [nanos]
  (when nanos (/ nanos 1e9)))

(defn- metrics
  [body]
  {:load-s (seconds (:load_duration body))
   :prompt-eval-s (seconds (:prompt_eval_duration body))
   :eval-s (seconds (:eval_duration body))
   :total-s (seconds (:total_duration body))
   :prompt-tokens (:prompt_eval_count body)
   :output-tokens (:eval_count body)
   :tokens-per-s (when (and (:eval_count body)
                            (pos? (or (:eval_duration body) 0)))
                   (/ (:eval_count body)
                      (seconds (:eval_duration body))))
   :done-reason (:done_reason body)})

(defn- parse-response
  [text]
  (try
    (json/parse-string text ->kebab-case-keyword)
    (catch Exception _ nil)))

(defn- warm-up
  "Loads the model with the given hardware settings, so the receipt runs
  don't include the load time. Returns the load time in seconds."
  [model hw-opts opts]
  (let [{:keys [status body]} (ollama/generate
                                {:model model
                                 :prompt "Reply with OK."
                                 :stream false
                                 :options (cond-> {:num_predict 1
                                                   :num_ctx (:num-ctx opts)}
                                            (:num-gpu hw-opts)
                                            (assoc :num_gpu (:num-gpu hw-opts)))}
                                opts)]
    {:status status
     :load-s (seconds (:load_duration body))}))

(defn- run-one
  [{:keys [image expected]} prompt schema opts]
  (let [started (System/nanoTime)
        result (try
                 (let [{:keys [status body]} (ollama/generate
                                               (ollama/request-body
                                                 (ollama/->base64 image)
                                                 prompt
                                                 schema
                                                 opts)
                                               opts)]
                   (cond
                     (not (<= 200 status 299))
                     {:error (str "HTTP " status ": " body)}

                     ; A runner failure (e.g., a grammar exception) can come
                     ; back as a 200 with an empty, unfinished body
                     (not (:done body))
                     {:error (str "Incomplete response: "
                                  (or (:error body) (pr-str body)))}

                     :else
                     (let [parsed (parse-response (:response body))]
                       {:response (:response body)
                        :parsed parsed
                        :valid-json (map? parsed)
                        :metrics (metrics body)
                        :score (when (map? parsed)
                                 (score expected parsed))})))
                 (catch Exception e
                   {:error (.getMessage e)}))]
    (assoc result :wall-s (seconds (- (System/nanoTime) started)))))

(defn- result-file
  [out-dir model hardware receipt seed]
  (io/file out-dir
           (safe-name model)
           hardware
           (format "%s-seed%s.edn" receipt seed)))

(defn- write-edn
  [file data]
  (io/make-parents file)
  (spit file (with-out-str (pprint data))))

(defn- fmt
  [v]
  (cond
    (nil? v) ""
    (float? v) (format "%.3f" v)
    (boolean? v) (if v "1" "0")
    :else (str v)))

(def ^:private csv-columns
  [:model :hardware :receipt :seed :error :valid-json :score :usable
   :location-name :location-address :date :total :tax :tax-rate :tip
   :payment-account :item-precision :item-recall :item-f1
   :account-accuracy :taxable-accuracy :hallucination-count :consistent
   :prompt-eval-s :eval-s :total-s :wall-s :prompt-tokens :output-tokens
   :tokens-per-s :done-reason])

(defn- flatten-result
  [{:keys [config score metrics] :as result}]
  (merge (select-keys config [:model :hardware :receipt :seed])
         (select-keys result [:error :valid-json :wall-s])
         ; A run that failed or returned unreadable output scores zero
         {:score 0.0 :usable false}
         (dissoc score :hallucinated)
         {:hallucination-count (some-> score :hallucinated count)}
         metrics))

(defn- read-results
  [out-dir]
  (->> (file-seq (io/file out-dir))
       (filter #(re-find #"-seed\d+\.edn$" (.getName %)))
       (map (comp flatten-result read-edn))
       (sort-by (juxt :model :hardware :receipt :seed))))

(defn- mean
  [xs]
  (let [xs (map #(cond (true? %) 1.0 (false? %) 0.0 :else %)
                (remove nil? xs))]
    (when (seq xs)
      (/ (reduce + xs) (double (count xs))))))

(defn- percentile
  [p xs]
  (let [xs (vec (sort (remove nil? xs)))]
    (when (seq xs)
      (xs (min (dec (count xs))
               (int (Math/floor (* p (count xs)))))))))

(defn- pct
  [v]
  (if v (format "%.0f%%" (* 100 v)) "–"))

(defn- num-cell
  [v]
  (if v (format "%.1f" (double v)) "–"))

(defn- md-table
  [headers rows]
  (str/join "\n"
            (concat [(str "| " (str/join " | " headers) " |")
                     (str "|" (str/join "|" (repeat (count headers) "---")) "|")]
                    (map #(str "| " (str/join " | " %) " |") rows))))

(defn- summary
  [results warm-ups]
  (let [groups (group-by (juxt :model :hardware) results)
        configs (sort (keys groups))
        receipts (sort (distinct (map :receipt results)))]
    (str "# Receipt evaluation\n\n"
         "Generated " (t/format "yyyy-MM-dd HH:mm" (t/local-date-time)) ". "
         "Score is a weighted average of the field and line-item measures: "
         (str/join ", " (map (fn [[k w]] (format "%s %.0f%%" (name k) (* 100 w)))
                             (sort-by (comp - val) weights)))
         ". Usable means the merchant, date, total, and payment account are "
         "right and every line item was found with nothing extra.\n\n"
         "## By configuration\n\n"
         (md-table ["Model" "HW" "Runs" "Score" "Usable" "Valid JSON"
                    "Total" "Date" "Merchant" "Tax" "Items F1" "Accounts"
                    "Halluc./run" "Adds up" "Median s" "P90 s" "Tok/s"
                    "Load s"]
                   (for [[model hw :as k] configs
                         :let [rs (groups k)]]
                     [model hw (count rs)
                      (pct (mean (map :score rs)))
                      (pct (mean (map :usable rs)))
                      (pct (mean (map #(boolean (:valid-json %)) rs)))
                      (pct (mean (map :total rs)))
                      (pct (mean (map :date rs)))
                      (pct (mean (map :location-name rs)))
                      (pct (mean (map :tax rs)))
                      (pct (mean (map :item-f1 rs)))
                      (pct (mean (map :account-accuracy rs)))
                      (num-cell (mean (map :hallucination-count rs)))
                      (pct (mean (map :consistent rs)))
                      (num-cell (percentile 0.5 (map :total-s rs)))
                      (num-cell (percentile 0.9 (map :total-s rs)))
                      (num-cell (mean (map :tokens-per-s rs)))
                      (num-cell (get-in warm-ups [k :load-s]))]))
         "\n\n## Score by receipt\n\n"
         (md-table (cons "Receipt" (map #(str/join " " %) configs))
                   (for [r receipts]
                     (cons r (for [k configs]
                               (pct (mean (map :score
                                               (filter #(= r (:receipt %))
                                                       (groups k)))))))))
         "\n")))

(defn- write-reports
  [out-dir]
  (let [results (read-results out-dir)
        warm-ups (->> (file-seq (io/file out-dir))
                      (filter #(= "warm-up.edn" (.getName %)))
                      (map read-edn)
                      (map (juxt (juxt :model :hardware) identity))
                      (into {}))]
    (with-open [w (io/writer (io/file out-dir "results.csv"))]
      (csv/write-csv w (cons (map name csv-columns)
                             (map (fn [r] (map (comp fmt r) csv-columns))
                                  results))))
    (spit (io/file out-dir "summary.md") (summary results warm-ups))
    (println "Wrote" (str (io/file out-dir "results.csv"))
             "and" (str (io/file out-dir "summary.md")))))

(defn evaluate
  [{:keys [models hardware repeats images expected accounts output]
    :as opts}]
  (let [receipts (find-receipts (map expand-home images)
                                (load-expected (expand-home expected)))
        schema (rcpts/build-schema (read-edn (expand-home accounts)))
        prompt (rcpts/prompt nil)
        models (if (seq models)
                 (map (fn [m] {:name m :capabilities (model-capabilities m opts)})
                      models)
                 (vision-models opts))
        hardware (if (= "both" hardware) ["gpu" "cpu"] [hardware])
        out-dir (io/file (expand-home output))]
    (println (format "%d receipt(s) × %d model(s) × %d hardware × %d seed(s) → %s"
                     (count receipts) (count models) (count hardware)
                     repeats (str out-dir)))
    (write-edn (io/file out-dir "prompt.edn")
               {:prompt prompt :schema schema :options (dissoc opts :args)})
    (doseq [{model :name :keys [capabilities]} models
            hw hardware
            :let [hw-opts (cond-> (merge opts (hardware-options hw) {:model model})
                            ; Reasoning first can use up the token budget
                            ; before any JSON is written
                            (and (some #{"thinking"} capabilities)
                                 (not (:think? opts)))
                            (assoc :think false))
                  warm-up-file (io/file out-dir (safe-name model) hw "warm-up.edn")]]
      (when-not (.exists warm-up-file)
        (println "Loading" model "on" hw "…")
        (write-edn warm-up-file (merge {:model model :hardware hw}
                                       (warm-up model hw-opts opts))))
      (doseq [receipt receipts
              seed (range 1 (inc repeats))
              :let [file (result-file out-dir model hw (:name receipt) seed)]]
        (if (.exists file)
          (println "Already done:" model hw (:name receipt) "seed" seed)
          (let [result (run-one receipt prompt schema (assoc hw-opts :seed seed))
                config {:model model
                        :hardware hw
                        :receipt (:name receipt)
                        :seed seed
                        :image (str (:image receipt))}]
            (write-edn file (assoc result :config config))
            (println (format "%s %s %s seed %d: %s in %.0fs"
                             model hw (:name receipt) seed
                             (if-let [e (:error result)]
                               (str "ERROR " e)
                               (str "score " (pct (get-in result [:score :score]))))
                             (:wall-s result)))
            ; Keep the reports current so a long run can be checked midway
            (write-reports out-dir)))))
    (write-reports out-dir)))

(def ^:private cli-options
  {:usage "lein eval-receipts -- <options>"
   :description "Evaluate vision models at reading receipts. Results that already exist in the output directory are skipped, so an interrupted run can be resumed by passing the same --output."
   :options (conj default-options
                  ["-m" "--model MODEL" "A model to evaluate (repeatable). Defaults to every installed vision model."
                   :id :models
                   :default []
                   :update-fn conj
                   :multi true]
                  ["-g" "--hardware HARDWARE" "gpu, cpu, or both"
                   :default "both"
                   :validate [#{"gpu" "cpu" "both"} "Must be gpu, cpu, or both"]]
                  ["-r" "--repeats N" "Runs per receipt, each with a different seed"
                   :default 1
                   :parse-fn parse-long]
                  ["-i" "--images DIR" "A directory of receipt images (repeatable)"
                   :id :images
                   :default ["~/Syncthing/receipts" "~/Desktop/money/receipts"]
                   :update-fn (fn [dirs dir]
                                (if (::custom-images (meta dirs))
                                  (conj dirs dir)
                                  (with-meta [dir] {::custom-images true})))
                   :multi true]
                  ["-e" "--expected DIR" "The directory of expected answers, one <image name>.edn per receipt"
                   :default "~/Desktop/money/receipts/expected"]
                  ["-a" "--accounts FILE" "An EDN file of the :payment-accounts and :expense-accounts offered to the model"
                   :default "~/Desktop/money/receipts/accounts.edn"]
                  ["-o" "--output DIR" "Where to write the results"
                   :default (str "~/Desktop/money/receipts/ingested/"
                                 (t/format "yyyyMMdd-HHmmss" (t/local-date-time)))]
                  ["-c" "--num-ctx N" "The context window size"
                   :default 8192
                   :parse-fn parse-long]
                  ["-p" "--num-predict N" "The maximum number of tokens to generate"
                   :default 1500
                   :parse-fn parse-long]
                  [nil "--host HOST" "The ollama host"
                   :default "localhost"]
                  [nil "--port PORT" "The ollama port"
                   :default 11434
                   :parse-fn parse-long]
                  [nil "--think" "Let thinking models reason before answering (off by default)"
                   :id :think?]
                  [nil "--report-only" "Only rebuild results.csv and summary.md in --output"
                   :id :report-only?])})

(defn run
  [& args]
  (with-options [parsed (assoc cli-options :args args)]
    (let [{:keys [report-only? output] :as opts} (:options parsed)
          opts (assoc opts :timeout-ms (* 30 60 1000))]
      (if report-only?
        (write-reports (io/file (expand-home output)))
        (evaluate opts))))
  (shutdown-agents))
