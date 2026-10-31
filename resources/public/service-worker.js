// Bump to discard caches written by earlier versions of this worker
const CACHE_VERSION = 'v2';
const SHELL_CACHE = 'shell-' + CACHE_VERSION;
const STATIC_CACHE = 'static-' + CACHE_VERSION;

const SHELL_URLS = [
  '/',
  '/css/site.css',
  '/js/bootstrap.min.js',
  '/images/logo.svg',
  '/images/icon-192.png',
  '/images/icon-512.png',
  '/cljs-out/clj-money.js'
];

const API_PREFIXES = ['/api/', '/oapi/', '/auth/', '/app/'];

// The server answers unknown paths with the app's HTML, so a static asset
// fetched while it was missing would otherwise be cached as HTML
const isCacheableAsset = response =>
  response.ok && !response.headers.get('Content-Type')?.includes('text/html');

const cacheAsset = (cache, url) =>
  fetch(url).then(response => {
    if (isCacheableAsset(response)) return cache.put(url, response);
  });

self.addEventListener('install', event => {
  event.waitUntil(
    Promise.all([
      caches.open(SHELL_CACHE).then(cache => cache.add('/').catch(() => {})),
      caches.open(STATIC_CACHE).then(cache =>
        Promise.allSettled(SHELL_URLS.map(url => cacheAsset(cache, url)))
      )
    ]).then(() => self.skipWaiting())
  );
});

self.addEventListener('activate', event => {
  const validCaches = [SHELL_CACHE, STATIC_CACHE];
  event.waitUntil(
    caches.keys()
      .then(keys => Promise.all(
        keys.filter(k => !validCaches.includes(k)).map(k => caches.delete(k))
      ))
      .then(() => self.clients.claim())
  );
});

self.addEventListener('fetch', event => {
  const {request} = event;
  const url = new URL(request.url);

  if (request.method !== 'GET' || url.origin !== self.location.origin) return;

  const path = url.pathname;

  if (API_PREFIXES.some(prefix => path.startsWith(prefix))) {
    event.respondWith(fetch(request));
    return;
  }

  if (request.headers.get('Accept')?.includes('text/html')) {
    // Network first, so a deploy's new shell is picked up right away
    event.respondWith(
      fetch(request)
        .then(response => {
          if (response.ok) {
            const copy = response.clone();
            caches.open(SHELL_CACHE).then(cache => cache.put('/', copy));
          }
          return response;
        })
        .catch(() => caches.match('/'))
    );
    return;
  }

  event.respondWith(
    caches.match(request).then(cached => {
      const networkFetch = fetch(request).then(response => {
        if (isCacheableAsset(response)) {
          // Clone before returning; the page consumes the original body
          const copy = response.clone();
          caches.open(STATIC_CACHE).then(cache => cache.put(request, copy));
        }
        return response;
      });
      return cached || networkFetch;
    })
  );
});
