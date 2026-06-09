const CACHE_VERSION = 'v1';
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

self.addEventListener('install', event => {
  event.waitUntil(
    Promise.all([
      caches.open(SHELL_CACHE).then(cache => cache.add('/').catch(() => {})),
      caches.open(STATIC_CACHE).then(cache =>
        Promise.allSettled(SHELL_URLS.map(url => cache.add(url)))
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
    event.respondWith(
      caches.match('/').then(cached => cached || fetch(request)).catch(() => caches.match('/'))
    );
    return;
  }

  event.respondWith(
    caches.match(request).then(cached => {
      const networkFetch = fetch(request).then(response => {
        if (response.ok) caches.open(STATIC_CACHE).then(cache => cache.put(request, response.clone()));
        return response;
      });
      return cached || networkFetch;
    })
  );
});
