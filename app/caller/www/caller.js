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
  function initialize() {
    if (window.jQuery) {
      window.jQuery(document).on('shiny:disconnected', () => { window.callerShinyDisconnected = true; update(); });
      window.jQuery(document).on('shiny:connected', () => { window.callerShinyDisconnected = false; update(); openTrackingLink(); });
    }
    update();
    // A dynamically loaded script can run after DOMContentLoaded or Shiny's
    // first connection event. Read the initial fragment now as well, so the
    // caller token is not dependent on which event happened first.
    openTrackingLink();
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', initialize, {once: true});
  else initialize();
})();
// Persistent map output: position updates move a marker without resetting zoom.
(() => {
  const binding = new Shiny.OutputBinding();
  binding.find = scope => $(scope).find('.caller-map-output');
  const icon = (character, kind) => L.divIcon({
    className: `caller-map-character ${kind}`,
    html: `<span aria-hidden="true">${character}</span>`,
    iconSize: [48, 48], iconAnchor: kind === 'caller-hospital' ? [-12, 24] : [24, 24]
  });
  binding.renderValue = (el, value) => {
    try {
    const data = JSON.parse(value);
    if (data && (!data.hospital || !Number.isFinite(data.hospital.lat) || !Number.isFinite(data.hospital.lng))) data.hospital = null;
    if (!data) {
      if (el.callerResize) el.callerResize.disconnect();
      if (el.callerMap) el.callerMap.remove();
      el.callerMap = null; el.callerIncident = null; el.callerRouteKey = null; el.callerHospital = null; el.callerRoute = null;
      el.replaceChildren();
      return;
    }
    if (!el.callerMap || el.callerIncident !== data.incident_id) {
      if (el.callerResize) el.callerResize.disconnect();
      if (el.callerMap) el.callerMap.remove();
      const map = L.map(el, {scrollWheelZoom: false});
      el.callerMap = map;
      el.callerIncident = data.incident_id;
      el.callerRouteKey = null; el.callerHospital = null; el.callerRoute = null;
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
    if (data.waiting_for_confirmation) el.callerAmbulance.remove();
    else el.callerAmbulance.addTo(el.callerMap);
    if (data.hospital) el.callerHouse.remove();
    else el.callerHouse.addTo(el.callerMap);
    // Route data is delivered only through the authorized incident lookup.
    // Switch geometry on confirmed hospital changes; retain user zoom on ticks.
    if (el.callerRouteKey !== data.route_key) {
      if (el.callerHospital) el.callerHospital.remove();
      if (el.callerRoute) el.callerRoute.remove();
      el.callerHospital = null; el.callerRoute = null;
      el.callerRouteKey = data.route_key;
      const routeStatus = el.parentElement.querySelector('.caller-route-status');
      if (data.hospital) {
        const hospital = data.hospital;
        const label = document.createElement('span');
        label.textContent = `${hospital.name} · confirmed fictional destination`;
        el.callerHospital = L.marker([hospital.lat, hospital.lng], {
          icon: icon('🏥', 'caller-hospital'), title: hospital.name,
          alt: 'Crew-confirmed fictional hospital', zIndexOffset: 100
        }).addTo(el.callerMap).bindTooltip(label, {permanent:true,direction:'right'});
        if (Array.isArray(data.hospital_route) && data.hospital_route.length >= 2) {
          el.callerRoute = L.polyline(data.hospital_route, {color:'#2468c4',weight:5,opacity:0.85})
            .addTo(el.callerMap).bindTooltip('Synthetic hospital trip · stored OSRM road snapshot, not live navigation');
          el.callerMap.fitBounds(el.callerRoute.getBounds().extend([hospital.lat,hospital.lng]), {padding:[48,48],maxZoom:15});
          if (routeStatus) routeStatus.textContent = `Route to ${hospital.name} · fictional destination, stored road snapshot. No live navigation or hospital ETA.`;
        } else {
          el.callerMap.fitBounds([[data.ambulance.lat,data.ambulance.lng],[hospital.lat,hospital.lng]], {padding:[48,48],maxZoom:15});
          if (routeStatus) routeStatus.textContent = 'Hospital confirmed; road route unavailable. No straight-line route substituted.';
        }
      } else if (routeStatus) routeStatus.textContent = 'Hospital route appears after crew confirmation.';
    }
    // Keep both characters visible when the ambulance reaches the same location.
    el.callerAmbulance.setIcon(icon('🚑', data.arrived ? 'caller-ambulance at-destination' : 'caller-ambulance'));
    } catch (error) {
      if (el.callerMap) el.callerMap.remove();
      el.callerMap = null; el.callerIncident = null; el.callerRouteKey = null;
      el.textContent = 'Map temporarily unavailable. Reconnect for updated location information.';
    }
  };
  document.addEventListener('click', event => {
    const button = event.target.closest('.caller-recenter');
    if (!button) return;
    const el = document.getElementById(button.dataset.map);
    if (!el || !el.callerMap || !el.callerLocations) return;
    const d = el.callerLocations;
    const bounds = el.callerRoute ? el.callerRoute.getBounds().extend([d.ambulance.lat,d.ambulance.lng]) :
      L.latLngBounds([[d.ambulance.lat,d.ambulance.lng],[d.destination.lat,d.destination.lng]]);
    if (d.hospital) bounds.extend([d.hospital.lat,d.hospital.lng]);
    el.callerMap.fitBounds(bounds, {padding: [48, 48], maxZoom: 16});
  });
  Shiny.outputBindings.register(binding, 'diis.callerMap');
})();
