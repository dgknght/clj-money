(ns clj-money.otel-helpers
  "Installs an in-memory OpenTelemetry SDK as the global instance, so that
  tests can assert on the spans and metrics the application produces.

  Spans are only sampled while telemetry is being captured (see capture), so
  that the many spans created by the rest of the test suite aren't held in
  memory. Instruments created with clj-money.otel/histogram are resolved
  lazily, so a histogram first used before this namespace is loaded is bound
  to the no-op implementation; require this namespace before using one."
  (:import io.opentelemetry.api.GlobalOpenTelemetry
           io.opentelemetry.api.common.Attributes
           io.opentelemetry.sdk.OpenTelemetrySdk
           [io.opentelemetry.sdk.trace SdkTracerProvider]
           [io.opentelemetry.sdk.trace.data SpanData EventData]
           io.opentelemetry.sdk.trace.export.SimpleSpanProcessor
           [io.opentelemetry.sdk.trace.samplers Sampler SamplingResult]
           [io.opentelemetry.sdk.metrics SdkMeterProvider]
           [io.opentelemetry.sdk.metrics.data
            MetricData
            MetricDataType
            PointData
            HistogramPointData
            LongPointData
            DoublePointData]
           io.opentelemetry.sdk.testing.exporter.InMemorySpanExporter
           io.opentelemetry.sdk.testing.exporter.InMemoryMetricReader))

(defonce ^:private capturing? (atom false))

(def ^:private sampler
  (reify Sampler
    (shouldSample [_ _parent-context _trace-id _name _kind _attributes _links]
      (if @capturing?
        (SamplingResult/recordAndSample)
        (SamplingResult/drop)))
    (getDescription [_] "CaptureSampler")))

(defonce ^:private span-exporter (InMemorySpanExporter/create))

; Delta temporality, so that each collection reports only what was
; recorded since the previous one
(defonce ^:private metric-reader (InMemoryMetricReader/createDelta))

(defonce ^:private sdk
  (let [sdk (.. (OpenTelemetrySdk/builder)
                (setTracerProvider
                  (.. (SdkTracerProvider/builder)
                      (setSampler sampler)
                      (addSpanProcessor (SimpleSpanProcessor/create span-exporter))
                      build))
                (setMeterProvider
                  (.. (SdkMeterProvider/builder)
                      (registerMetricReader metric-reader)
                      build))
                build)]
    ; Anything that has called GlobalOpenTelemetry/get already has
    ; fixed the global instance to the no-op implementation
    (GlobalOpenTelemetry/resetForTest)
    (GlobalOpenTelemetry/set sdk)
    sdk))

(defn- attributes->map
  [^Attributes attributes]
  (update-keys (into {} (.asMap attributes))
               #(.getKey ^io.opentelemetry.api.common.AttributeKey %)))

(defn- event->map
  [^EventData event]
  {:name (.getName event)
   :attributes (attributes->map (.getAttributes event))})

(defn- span->map
  [^SpanData span]
  (let [status (.getStatus span)]
    {:name (.getName span)
     :span-id (.getSpanId span)
     :parent-span-id (when (.isValid (.getParentSpanContext span))
                       (.getSpanId (.getParentSpanContext span)))
     :attributes (attributes->map (.getAttributes span))
     :status (keyword (.toLowerCase (str (.getStatusCode status))))
     :status-description (.getDescription status)
     :events (mapv event->map (.getEvents span))}))

(defn- point->map
  [^PointData point]
  (merge {:attributes (attributes->map (.getAttributes point))}
         (condp instance? point
           HistogramPointData {:count (.getCount ^HistogramPointData point)
                               :sum (.getSum ^HistogramPointData point)
                               :boundaries (vec (.getBoundaries ^HistogramPointData point))}
           LongPointData {:value (.getValue ^LongPointData point)}
           DoublePointData {:value (.getValue ^DoublePointData point)})))

(defn- metric->map
  [^MetricData metric]
  {:name (.getName metric)
   :description (.getDescription metric)
   :unit (.getUnit metric)
   :type (condp = (.getType metric)
           MetricDataType/HISTOGRAM :histogram
           MetricDataType/LONG_GAUGE :long-gauge
           MetricDataType/DOUBLE_GAUGE :double-gauge
           (keyword (.toLowerCase (str (.getType metric)))))
   :points (mapv point->map (.getPoints (.getData metric)))})

(defn collect-metrics
  "Returns the metrics recorded since the previous collection, as a map of
  instrument name to metric map (:description, :unit, :type and :points)"
  []
  (->> (.collectAllMetrics ^InMemoryMetricReader metric-reader)
       (map (comp (juxt :name identity) metric->map))
       (into {})))

(defn capture
  "Calls f, and returns a map with the :result of the call (or the
  :exception it threw), the :spans that ended during the call, in the order
  they ended, and the :metrics recorded during the call (see
  collect-metrics)"
  [f]
  (assert sdk "The in-memory OpenTelemetry SDK is not installed")
  (locking capturing?
    (.reset ^InMemorySpanExporter span-exporter)
    (collect-metrics)
    (reset! capturing? true)
    (try
      (let [outcome (try
                      {:result (f)}
                      (catch Exception e
                        {:exception e}))]
        (assoc outcome
               :spans (mapv span->map (.getFinishedSpanItems ^InMemorySpanExporter span-exporter))
               :metrics (collect-metrics)))
      (finally
        (reset! capturing? false)
        (.reset ^InMemorySpanExporter span-exporter)))))
