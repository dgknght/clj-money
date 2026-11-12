// Bump to discard caches written by earlier versions of this worker
const CACHE_VERSION = 'v3';
const SHELL_CACHE = 'shell-' + CACHE_VERSION;
const STATIC_CACHE = 'static-' + CACHE_VERSION;

// The app's script and styles aren't listed here: the shell requests them
// with a hash of their content in the query string (?v=...), which changes
// with each build. They're cached the first time the shell requests them.
const SHELL_URLS = [
  '/',
  '/images/logo.svg',
  '/images/icon-192.png',
  '/images/icon-512.png'
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

// Caches the response for a versioned asset and drops the earlier builds'
// copies of it, so they don't pile up with each deploy
const replaceVersions = (cache, request, response) => {
  const path = new URL(request.url).pathname;
  return cache.keys()
    .then(keys => Promise.all(
      keys.filter(key => new URL(key.url).pathname === path)
        .map(key => cache.delete(key))
    ))
    .then(() => cache.put(request, response));
};

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

  // A versioned asset never changes at its URL, so a cached copy needs no
  // refresh
  if (url.searchParams.has('v')) {
    event.respondWith(
      caches.match(request).then(cached => cached || fetch(request).then(response => {
        if (isCacheableAsset(response)) {
          const copy = response.clone();
          caches.open(STATIC_CACHE).then(cache => replaceVersions(cache, request, copy));
        }
        return response;
      }))
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
