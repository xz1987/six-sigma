// Returning home must remove the private caller fragment before a new session.
(() => {
  function returnHome(_message) {
    window.history.replaceState(null, '', window.location.pathname + window.location.search);
    window.location.reload();
  }
  // Work even before Shiny connects, or after its connection is interrupted.
  document.addEventListener('click', event => {
    const button = event.target.closest && event.target.closest('#change_role');
    if (!button) return;
    event.preventDefault();
    event.stopImmediatePropagation();
    returnHome();
  }, true);
  function register() {
    if (!window.Shiny) return;
    window.Shiny.addCustomMessageHandler('diis-return-home', returnHome);
    window.Shiny.addCustomMessageHandler('diis-active-trip', data => {
      if(!/^#incident=[a-f0-9]{64}$/.test(data.fragment)) return;
      try { sessionStorage.setItem('diis-active-trip',JSON.stringify(data)); } catch (_) {}
    });
    window.Shiny.addCustomMessageHandler('diis-clear-active-trip', () => {
      try { sessionStorage.removeItem('diis-active-trip'); } catch (_) {}
    });
    window.Shiny.addCustomMessageHandler('diis-open-active-trip', () => {
      // Keep an explicitly opened private link; never replace it with another trip.
      if(new URLSearchParams(window.location.hash.slice(1)).has('incident')) return;
      try {
        const data=JSON.parse(sessionStorage.getItem('diis-active-trip') || 'null');
        if(!data || data.expires<=Date.now() || !/^#incident=[a-f0-9]{64}$/.test(data.fragment)) return;
        window.location.hash=data.fragment;
      } catch (_) {}
    });
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', register);
  else register();
})();
