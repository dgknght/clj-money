// Entry point for the Chart.js bundle. esbuild bundles this into
// resources/js/chart-bundle.js (see "build:chart" in package.json), which the
// ClojureScript build loads as a foreign lib (see src/deps.cljs). Keep the
// exported surface small: every property accessed from ClojureScript must
// also be declared in src/chart-externs.js to survive advanced compilation.

import {
  BarController,
  BarElement,
  CategoryScale,
  Chart,
  Colors,
  Legend,
  LinearScale,
  LineController,
  LineElement,
  PointElement,
  Tooltip,
} from 'chart.js';
import ChartDataLabels from 'chartjs-plugin-datalabels';

Chart.register(
  BarController,
  BarElement,
  CategoryScale,
  Colors,
  Legend,
  LinearScale,
  LineController,
  LineElement,
  PointElement,
  Tooltip,
  ChartDataLabels,
);

// The Colors plugin stands down once the default border color changes, which
// applyTheme does, leaving every dataset a dark grey.
Chart.defaults.plugins.colors.forceOverride = true;

// Every value charted in the app is an amount of money.
Chart.defaults.plugins.datalabels.formatter = (value) =>
  value.toLocaleString(undefined, {
    minimumFractionDigits: 2,
    maximumFractionDigits: 2,
  });

// Picks up the text and border colors of the current Bootstrap theme.
function applyTheme() {
  const style = getComputedStyle(document.body);
  Chart.defaults.color = style.getPropertyValue('--bs-body-color');
  Chart.defaults.borderColor = style.getPropertyValue('--bs-border-color');
}

// Draws a chart on the given canvas and returns it, to be passed back to
// update and destroy.
export function create(canvas, config) {
  applyTheme();
  return new Chart(canvas, config);
}

// Replaces the chart's type, data and options and redraws it.
export function update(chart, config) {
  chart.config.type = config.type;
  chart.data = config.data;
  chart.options = config.options;
  chart.update();
}

export function destroy(chart) {
  chart.destroy();
}
