# Real hospital directory in a simulated trip

Reviewed 2026-10-17. Nine selected facilities serve the five demo neighborhoods;
this is not an exhaustive hospital directory. Elmhurst and Cohen Children’s are
nearby specialty options outside the five named demo zones. No zone boundary
filters hospital selection. Matching published services are sorted by stored
road distance; this is not a clinical ranking or live destination recommendation.

`app/www/hospital-directory.json` stores claim-level source links, review date,
address, neighborhood, age group, designation and service summary.

- NYS trauma program: https://www.health.ny.gov/professionals/ems/state_trauma/trauma2.htm
- NYS facility addresses/geocodes: https://health.data.ny.gov/Health/Health-Facility-General-Information/vn5v-hh5r
- NYS stroke list: https://www.health.ny.gov/diseases/cardiovascular/stroke/designation/stroke_designated_centers.htm
- Cohen Children's address comes from the NYS pediatric trauma list; its own
  street-address geocode comes from https://geocoding.geo.census.gov/geocoder/.
  PFI 1630 is shared with the LIJ campus; the directory does not substitute the
  adult campus coordinates or adult designation for the pediatric program.

Trauma assessment matches the adult designated trauma facilities documented here; pediatric trauma
matches the designated pediatric program. Pediatric emergency assessment may
also match verified all-age emergency departments. Stroke and cardiac categories
are broad published services, not a claim that all listed centers have equal
interventional capabilities. Full destination suitability remains with crew and
dispatch under their protocols. Absence from a category means not documented in
this selected directory, not proof a hospital cannot provide that service.

Routes are OSRM/OpenStreetMap driving snapshots from synthetic scenes to facility
address geocodes. These are approximate hospital locations, not verified ambulance
entrances. Their endpoints may snap to nearby roads. They have no live traffic,
capacity or diversion status. Simulated eight-minute transport is independent of
route length and is not an estimated real travel time. Clinical and location
information must be refreshed before operational use.

To refresh all 45 route snapshots after reviewing the directory, run:
`python3 scripts/update-hospital-routes.py` from the repo root. The script requires
network access and curl; it does not send patient data.
