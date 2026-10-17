(() => {
  let installPrompt;
  const script = document.currentScript;
  const root = new URL('./', script.src);
  function ready() {
    const install = document.getElementById('supervisor-install-app');
    const installHelp = document.getElementById('supervisor-install-help');
    const installStatus = document.getElementById('supervisor-pwa-status');
    const status = document.getElementById('connection-status');
    if (!install || !status) return;
    function installed() {
      return window.matchMedia('(display-mode: standalone)').matches ||
        window.navigator.standalone === true;
    }
    function updateInstall() {
      install.hidden = false;
      install.disabled = installed();
      install.textContent = installed() ? 'App installed' : 'Install app';
      if (installStatus) installStatus.textContent = installed() ?
        'This dashboard is running as an installed app.' : '';
    }
    function updateConnection() {
      status.hidden = navigator.onLine;
    }
    window.addEventListener('online', updateConnection);
    window.addEventListener('offline', updateConnection);
    updateConnection();
    window.addEventListener('beforeinstallprompt', event => {
      event.preventDefault();
      installPrompt = event;
      updateInstall();
    });
    install.addEventListener('click', async () => {
      if (!installPrompt) {
        if (installHelp) installHelp.open = !installHelp.open;
        return;
      }
      const prompt = installPrompt;
      installPrompt = null;
      try {
        await prompt.prompt();
        const choice = await prompt.userChoice;
        if (choice && choice.outcome === 'dismissed' && installStatus)
          installStatus.textContent = 'Installation was cancelled. You can try again from the browser menu.';
      } finally { updateInstall(); }
    });
    window.addEventListener('appinstalled', () => {
      installPrompt = null;
      updateInstall();
    });
    updateInstall();
    if ('serviceWorker' in navigator && window.isSecureContext) {
      navigator.serviceWorker.register(new URL('sw.js', root), {scope: root.pathname})
        .catch(() => {
          if (installStatus) installStatus.textContent =
            'Offline page unavailable. You can continue using the connected website.';
        });
    }
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', ready);
  else ready();
})();
