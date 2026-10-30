;; The OpenTelemetry browser SDK, bundled from src/js/otel.js by `npm install`
;; (see "build:otel" in package.json)
{:foreign-libs [{:file "js/otel-bundle.js"
                 :provides ["opentelemetry"]
                 :global-exports {opentelemetry cljMoneyOtel}}]
 :externs ["otel-externs.js"]}
