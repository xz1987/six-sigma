(() => {
  let installPrompt;
  const script = document.currentScript;
  const root = new URL('./', script.src);
  function ready() {
    const install = document.getElementById('install-app');
    const status = document.getElementById('connection-status');
    function updateConnection() {
      status.hidden = navigator.onLine;
    }
    window.addEventListener('online', updateConnection);
    window.addEventListener('offline', updateConnection);
    updateConnection();
    window.addEventListener('beforeinstallprompt', event => {
      event.preventDefault();
      installPrompt = event;
      install.hidden = false;
    });
    install.addEventListener('click', async () => {
      if (!installPrompt) return;
      install.hidden = true;
      try { await installPrompt.prompt(); await installPrompt.userChoice; }
      finally { installPrompt = null; }
    });
    window.addEventListener('appinstalled', () => {
      install.hidden = true;
      installPrompt = null;
    });
    if ('serviceWorker' in navigator && window.isSecureContext) {
      navigator.serviceWorker.register(new URL('sw.js', root), {scope: root.pathname})
        .catch(() => {
          document.getElementById('pwa-setup-status').textContent =
            'Offline page unavailable. You can continue using the connected website.';
        });
    }
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', ready);
  else ready();
})();
