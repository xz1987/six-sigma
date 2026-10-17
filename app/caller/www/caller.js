(() => {
  const update = () => document.documentElement.classList.toggle('caller-disconnected', !navigator.onLine || Boolean(window.callerShinyDisconnected));
  function openTrackingLink() {
    if (!window.Shiny || !window.Shiny.setInputValue) return;
    // Fragment stays out of HTTP request URLs/referrer headers and service-worker caches.
    const token = new URLSearchParams(window.location.hash.slice(1)).get('incident');
    window.Shiny.setInputValue('trackingLinkOpened', Boolean(token), {priority: 'event'});
    window.Shiny.setInputValue('clientReady', true, {priority: 'event'});
    window.Shiny.setInputValue('caller-linkToken', token || '', {priority: 'event'});
  }
  window.addEventListener('online', update);
  window.addEventListener('offline', update);
  window.addEventListener('hashchange', openTrackingLink);
  document.addEventListener('DOMContentLoaded', () => {
    if (window.jQuery) {
      window.jQuery(document).on('shiny:disconnected', () => { window.callerShinyDisconnected = true; update(); });
      window.jQuery(document).on('shiny:connected', () => { window.callerShinyDisconnected = false; update(); openTrackingLink(); });
    }
    update();
  });
})();
// Persistent map output: position updates move a marker without resetting zoom.
(() => {
  const binding = new Shiny.OutputBinding();
  binding.find = scope => $(scope).find('.caller-map-output');
  const icon = (character, kind) => L.divIcon({
    className: `caller-map-character ${kind}`,
    html: `<span aria-hidden="true">${character}</span>`,
    iconSize: [48, 48], iconAnchor: [24, 24]
  });
  binding.renderValue = (el, value) => {
    const data = JSON.parse(value);
    if (!data) {
      if (el.callerResize) el.callerResize.disconnect();
      if (el.callerMap) el.callerMap.remove();
      el.callerMap = null; el.callerIncident = null;
      el.replaceChildren();
      return;
    }
    if (!el.callerMap || el.callerIncident !== data.incident_id) {
      if (el.callerResize) el.callerResize.disconnect();
      if (el.callerMap) el.callerMap.remove();
      const map = L.map(el, {scrollWheelZoom: false});
      el.callerMap = map;
      el.callerIncident = data.incident_id;
      const status = el.parentElement.querySelector('.caller-tile-status');
      const tiles = L.tileLayer('https://tile.openstreetmap.org/{z}/{x}/{y}.png', {
        maxZoom: 19,
        attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
      }).addTo(map);
      tiles.on('tileerror', () => { status.textContent = 'Street map could not load. Check your connection; marker locations are simulated.'; });
      tiles.on('tileload', () => { status.textContent = ''; });
      el.callerHouse = L.marker([data.destination.lat, data.destination.lng], {
        icon: icon('🏠', 'caller-house'), title: 'Your incident location (synthetic)', alt: 'House symbol: your incident location', zIndexOffset: 100
      }).addTo(map).bindTooltip('Your incident location · synthetic');
      el.callerAmbulance = L.marker([data.ambulance.lat, data.ambulance.lng], {
        icon: icon('🚑', 'caller-ambulance'), title: 'Your assigned ambulance (simulated)', alt: 'Ambulance: your assigned unit', zIndexOffset: 200
      }).addTo(map).bindTooltip('Your assigned ambulance · simulated GPS');
      map.fitBounds([[data.ambulance.lat, data.ambulance.lng], [data.destination.lat, data.destination.lng]], {padding: [48, 48], maxZoom: 15});
      // Conditional-panel visibility and resizing must not leave a blank map.
      el.callerResize = new ResizeObserver(() => map.invalidateSize());
      el.callerResize.observe(el);
    }
    el.callerLocations = data;
    el.classList.toggle('caller-location-stale', data.stale);
    el.callerAmbulance.setLatLng([data.ambulance.lat, data.ambulance.lng]);
    el.callerHouse.setLatLng([data.destination.lat, data.destination.lng]);
    // Keep both characters visible when the ambulance reaches the same location.
    el.callerAmbulance.setIcon(icon('🚑', data.arrived ? 'caller-ambulance at-destination' : 'caller-ambulance'));
  };
  document.addEventListener('click', event => {
    const button = event.target.closest('.caller-recenter');
    if (!button) return;
    const el = document.getElementById(button.dataset.map);
    if (!el || !el.callerMap || !el.callerLocations) return;
    const d = el.callerLocations;
    el.callerMap.fitBounds([[d.ambulance.lat, d.ambulance.lng], [d.destination.lat, d.destination.lng]], {padding: [48, 48], maxZoom: 16});
  });
  Shiny.outputBindings.register(binding, 'diis.callerMap');
})();
