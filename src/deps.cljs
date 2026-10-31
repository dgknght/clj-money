;; Browser libraries bundled from src/js by `npm install` (see the "build:*"
;; scripts in package.json)
{:foreign-libs [{:file "js/otel-bundle.js"
                 :provides ["opentelemetry"]
                 :global-exports {opentelemetry cljMoneyOtel}}
                {:file "js/chart-bundle.js"
                 :provides ["chartjs"]
                 :global-exports {chartjs cljMoneyChart}}]
 :externs ["otel-externs.js"
           "chart-externs.js"]}
