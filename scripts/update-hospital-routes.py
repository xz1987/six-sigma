"""Refresh stored OSRM routes from synthetic scenes to real hospital address geocodes.
No live dispatch or clinical routing. Run from the repository root.
"""
import concurrent.futures
import copy
import json
import subprocess
from pathlib import Path

root = Path('app/www')
trips = json.loads((root / 'demo-zone-trips.json').read_text())
hospitals = json.loads((root / 'hospital-directory.json').read_text())['hospitals']

def refresh(item):
    zone, fixture = item
    scene = fixture['scene']['geometry']['coordinates'][-1]
    routes, facilities = {}, []
    for hospital in hospitals:
        url = (f'https://router.project-osrm.org/route/v1/driving/'
               f'{scene[0]},{scene[1]};{hospital["lng"]},{hospital["lat"]}'
               '?overview=full&geometries=geojson')
        data = json.loads(subprocess.check_output([
            'curl', '--fail', '--silent', '--show-error', '--retry', '2', '--max-time', '40', url]))
        route = data['routes'][0]
        feature = dict(type='Feature', geometry=route['geometry'], properties=dict(
            distance_m=route['distance'], source='OSRM / OpenStreetMap driving snapshot',
            retrieved_date='2026-10-17', synthetic_scene=True, real_hospital=True,
            entrance_verified=False, live_traffic=False))
        h = copy.deepcopy(hospital)
        h['distance_m'] = route['distance']
        facilities.append(h)
        routes[h['id']] = feature
    return zone, dict(scene=fixture['scene'], hospitals=facilities, hospital_routes=routes)

with concurrent.futures.ThreadPoolExecutor(max_workers=2) as executor:
    refreshed = dict(executor.map(refresh, trips.items()))
(root / 'demo-zone-trips.json').write_text(json.dumps(refreshed))
# Default Astoria routes support the standalone service tests as well.
for hospital_id, route in refreshed['Astoria']['hospital_routes'].items():
    (root / f'hospital-route-{hospital_id}.json').write_text(json.dumps(route))
print(f'Saved {len(hospitals)*len(trips)} road routes to {len(hospitals)} real hospitals across five zones')
