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
    if (window.Shiny) window.Shiny.addCustomMessageHandler('diis-return-home', returnHome);
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', register);
  else register();
})();
