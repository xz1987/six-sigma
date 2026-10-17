/* Synthetic positions; OSRM road snapshots; NYC NTA app-zone groupings. */
(() => {
  let map, movement, zones, hospitalsLayer, lastRoute, latest;
  const routePromises = new Map();
  const root = new URL('./', document.currentScript.src);
  async function getJSON(path) {
    const response = await fetch(new URL(path, root));
    if (!response.ok) throw Error(path);
    return response.json();
  }
  let zoneTrips;
  function getZoneTrips() {
    if(!zoneTrips) zoneTrips=getJSON('demo-zone-trips.json').catch(error=>{zoneTrips=null;throw error;});
    return zoneTrips;
  }
  function roadRoute(key, zone) {
    if(zone) return getZoneTrips().then(trips=>key==='scene'?trips[zone].scene:trips[zone].hospital_routes[key]);
    if (!routePromises.has(key)) {
      routePromises.set(key, getJSON(key === 'scene' ? 'demo-road-route.json' : `hospital-route-${key}.json`)
        .then(feature => {
          if (feature.geometry.type !== 'LineString' || feature.geometry.coordinates.length < 3) throw Error('Invalid road geometry');
          return feature;
        }).catch(error => {routePromises.delete(key);throw error;}));
    }
    return routePromises.get(key);
  }
  function position(points, fraction) {
    const lengths=points.slice(1).map((p,i)=>map.distance(points[i],p));
    let remaining=Math.max(0,Math.min(1,fraction))*lengths.reduce((a,b)=>a+b,0);
    for(let i=0;i<lengths.length;i++) {
      if(remaining<=lengths[i] && lengths[i]>0) {
        const f=remaining/lengths[i];
        return [points[i][0]+(points[i+1][0]-points[i][0])*f,points[i][1]+(points[i+1][1]-points[i][1])*f];
      }
      remaining-=lengths[i];
    }
    return points[points.length-1];
  }
  function init() {
    if(!window.Shiny || !window.L) return;
    Shiny.addCustomMessageHandler('responder-map', data => {
      const target=document.getElementById('responder-map');
      if(!target) return;
      latest=data;
      if(!map || map.getContainer()!==target) {
        if(map) map.remove();
        map=L.map(target).setView([40.765,-73.922],14);
        L.tileLayer('https://tile.openstreetmap.org/{z}/{x}/{y}.png',{
          maxZoom:19,attribution:'&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> · Zones: NYC DCP NTAs'
        }).addTo(map);
        movement=L.layerGroup().addTo(map);
        hospitalsLayer=L.layerGroup().addTo(map);
        lastRoute=null;
        const activeMap=map;
        getJSON('demo-zones.geojson').then(geo => {
          if(map!==activeMap) return;
          zones=L.geoJSON(geo,{
            style:feature=>({color:feature.properties.color,weight:3,opacity:1,fillColor:feature.properties.color,fillOpacity:0.05}),
            onEachFeature:(feature,layer)=>layer.bindTooltip(feature.properties.zone,{permanent:true,direction:'center',className:'zone-label'})
          }).addTo(map);
          zones.bringToBack();
        }).catch(()=>{
          const status=document.getElementById('map-zones-status');
          if(status) status.textContent='Zone boundaries unavailable.';
        });
        getZoneTrips().then(trips => {
          const data={hospitals:trips[latest.zone || 'Astoria'].hospitals};
          if(map!==activeMap) return;
          data.hospitals.forEach(h=>L.marker([h.lat,h.lng],{
            icon:L.divIcon({className:'hospital-map-icon',html:'<span aria-label="Hospital">H</span>',iconSize:[26,26],iconAnchor:[13,13]})
          }).bindPopup(`${h.name} · Published services<br>${h.capabilities.join(', ')}`).addTo(hospitalsLayer));
        }).catch(()=>{});
        document.getElementById('show-all-zones').onclick=()=>{
          if(zones) map.fitBounds(zones.getBounds(),{padding:[20,20]});
        };
        document.getElementById('focus-route').onclick=()=>{
          if(lastRoute) map.fitBounds(lastRoute.getBounds(),{padding:[24,24]});
        };
      }
      const activeMap=map;
      const key=data.hospital || 'scene';
      roadRoute(key,data.zone).then(feature=>{
        if(map!==activeMap || document.getElementById('responder-map')!==target) return;
        if((latest.hospital || 'scene')!==key || latest.zone!==data.zone) return;
        movement.clearLayers();
        const points=feature.geometry.coordinates.map(p=>[p[1],p[0]]);
        const fraction=key==='scene'?latest.elapsed/720:latest.travel/480;
        const current=position(points,fraction);
        const changed=!lastRoute || lastRoute._demoKey!==key;
        lastRoute=L.polyline(points,{color:'#2468c4',weight:5,opacity:0.85})
          .bindPopup('OSRM road snapshot · No live traffic, weather, or emergency-vehicle routing.').addTo(movement);
        lastRoute._demoKey=key;
        if(key==='scene') L.circleMarker(points[points.length-1],{radius:11,color:'#D15B4A',fillColor:'#D15B4A',fillOpacity:1})
          .bindPopup('Synthetic incident scene').addTo(movement);
        else L.circleMarker(points[0],{radius:11,color:'#D15B4A',fillColor:'#D15B4A',fillOpacity:1})
          .bindPopup('Synthetic incident scene / transport origin').addTo(movement);
        L.circleMarker(current,{radius:9,color:'#135b63',fillColor:'#135b63',fillOpacity:1})
          .bindPopup('Q-12 · simulated responder position').addTo(movement);
        document.getElementById('map-status').textContent=
          `${key==='scene'?'To incident':'To hospital'}: ${(feature.properties.distance_m/1609.344).toFixed(2)} miles · Road-route snapshot.`;
        if(changed) map.fitBounds(points,{padding:[24,24]});
        map.invalidateSize();
      }).catch(()=>{
        if(document.getElementById('map-status')) document.getElementById('map-status').textContent='Road route unavailable. No straight-line route substituted.';
      });
    });
  }
  if(document.readyState==='loading') document.addEventListener('DOMContentLoaded',init); else init();
})();
