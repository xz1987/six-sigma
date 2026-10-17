/* Only public static assets are cached. Never cache Shiny sessions or records. */
const CACHE_PREFIX = 'diis-ems-public-';
const CACHE_NAME = `${CACHE_PREFIX}v4`;
const ROOT = new URL('./', self.location.href);
const PUBLIC_ASSETS = ['offline.html', 'manifest.webmanifest', 'icons/diis-icon-192.png',
  'icons/diis-icon-512.png', 'icons/diis-icon-maskable-512.png', 'icons/diis-apple-touch-icon.png'];
self.addEventListener('install', (event) => {
  event.waitUntil(caches.open(CACHE_NAME).then(cache =>
    cache.addAll(PUBLIC_ASSETS.map(path => new URL(path, ROOT).href))));
});
self.addEventListener('activate', (event) => {
  event.waitUntil((async () => {
    const names = await caches.keys();
    await Promise.all(names.filter(name => name.startsWith(CACHE_PREFIX) && name !== CACHE_NAME)
      .map(name => caches.delete(name)));
    await self.clients.claim();
  })());
});
self.addEventListener('fetch', (event) => {
  const request = event.request;
  const url = new URL(request.url);
  // No interception of POSTs, downloads, websockets, or Shiny session endpoints.
  if (request.method !== 'GET' || url.origin !== ROOT.origin || request.mode !== 'navigate' ||
      url.pathname !== ROOT.pathname) return;
  event.respondWith((async () => {
    try {
      const response = await fetch(request);
      if (response.status < 500) return response;
      return (await caches.match(new URL('offline.html', ROOT).href)) || response;
    } catch (error) {
      return (await caches.match(new URL('offline.html', ROOT).href)) ||
        new Response('Connection unavailable. Reconnect and reopen the app.', {
          status: 503, headers: {'Content-Type': 'text/plain; charset=utf-8'}
        });
    }
  })());
});
