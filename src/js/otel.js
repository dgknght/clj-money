// Entry point for the OpenTelemetry browser bundle. esbuild bundles this
// into resources/js/otel-bundle.js (see "build:otel" in package.json), which
// the ClojureScript build loads as a foreign lib (see src/deps.cljs). Keep
// the exported surface small: every property accessed from ClojureScript
// must also be declared in src/otel-externs.js to survive advanced
// compilation.

import { context, propagation, trace } from '@opentelemetry/api';
import { getWebAutoInstrumentations } from '@opentelemetry/auto-instrumentations-web';
import {
  CompositePropagator,
  W3CBaggagePropagator,
  W3CTraceContextPropagator,
} from '@opentelemetry/core';
import { OTLPTraceExporter } from '@opentelemetry/exporter-trace-otlp-http';
import { registerInstrumentations } from '@opentelemetry/instrumentation';
import { resourceFromAttributes } from '@opentelemetry/resources';
import {
  BatchSpanProcessor,
  StackContextManager,
  TracerProvider,
} from '@opentelemetry/sdk-trace';

// opts: {url: <OTLP/HTTP traces endpoint>, serviceName: <string>}
export function init(opts) {
  const provider = new TracerProvider({
    resource: resourceFromAttributes({ 'service.name': opts.serviceName }),
    spanProcessors: [
      new BatchSpanProcessor({
        exporter: new OTLPTraceExporter({ url: opts.url }),
      }),
    ],
  });
  trace.setGlobalTracerProvider(provider);
  context.setGlobalContextManager(new StackContextManager().enable());
  propagation.setGlobalPropagator(
    new CompositePropagator({
      propagators: [
        new W3CTraceContextPropagator(),
        new W3CBaggagePropagator(),
      ],
    }),
  );

  // fetch and XHR spans add a traceparent header to same-origin requests,
  // which links them to the server-side spans from the Java agent.
  registerInstrumentations({
    instrumentations: [getWebAutoInstrumentations()],
  });
}
