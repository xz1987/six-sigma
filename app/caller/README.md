# Caller PWA module (H-16)

Matches the pushed EMS shell: teal #135b63, pale background, white rounded cards,
system type and the DIIS header. This isolated preview avoids overwriting the
other computer's staff interfaces or the local QC integration.

Run from the repository root:

```sh
Rscript app/caller/testme.R
```

Open http://127.0.0.1:5770. Requires R packages shiny, openssl and jsonlite (included with Shiny dependencies).
Click **Create simulated dispatch**, then **Open caller tracking**. Creating the
dispatch automatically generates a private link; callers enter no password or code.
The link also works in another tab/session of the same running server. Its fragment
is not sent in HTTP navigation requests or referrer headers. Keep the link private.
It expires after 30 minutes; restarting the R process invalidates every demo link.
Response advances automatically: assigned at 0s, approaching at 65s, confirmed
on scene at 90s. Create a new dispatch for a fresh replay. This is a synthetic
preview harness; no SMS is sent or actual telephone/dispatch provider contacted.
Optional `DIIS_CALLER_PORT` selects a different local preview port.

Implemented: server-scoped incident lookup, status, Queens street map with ambulance/house markers, last update,
arrival confirmation, disconnected/stale state, and existing PWA shell assets.
ETA deliberately remains unavailable until H-11/H-12 supplies a validated model.
Map uses real OpenStreetMap street tiles centered on synthetic coordinates in Queens.
Ambulance (🚑) and incident house (🏠) markers use geographic latitude/longitude.
Movement is scripted interpolation, not live GPS, driving directions or a road route.
The map supports pan/zoom and preserves the viewport between updates.
The timestamp is receipt time from the synthetic provider, not proof of GPS freshness.
No caller records, tokens, or tracking screens are service-worker cached.
Offline startup uses the public fallback; reconnection requires the Shiny server.
Install using browser installation controls; Safari uses Share > Add to Home Screen
or File > Add to Dock. Hosted HTTPS and device installation still need verification.

## Merge with the staff app

1. Copy caller.R into the merged app (or source `caller/caller.R`).
2. Copy caller.css, caller.js and vendor/leaflet/ into the shared app's www folder. Keep the existing
   shared manifest/service worker/pwa.js, rather than registering a second worker.
3. Replace only the caller placeholder with `caller_ui('caller')`.
4. The caller link handler in caller.js uses module ID `caller`. Keep that ID,
   or adjust its `caller-linkToken` input when using another namespace.
   In server call `caller_server('caller', lookup = function(token) { ... })`.
   The provider must validate an unexpired access grant and return ONLY its incident,
   or NULL for invalid, expired or revoked access. Use `caller_lookup` as the helper.
   Never authorize using an incident ID supplied by the browser.
5. The dispatcher creates the incident and access together using `caller_create_dispatch(store)` in this demo, or `caller_issue_access(incident_id)` for the shared incident store;
   deliver it privately to that caller. Do not put it in logs, files or public assets.
   Keep grants in a server-side store shared with the dispatcher. This preview uses
   process memory shared across sessions, so app restart invalidates all preview access.
   Multi-process deployment needs a shared server-side store, expiry and revocation.
6. The provider uses actual shared assignment/approach/on-scene events and location
   timestamps. `caller_replay` is demo-only. It must not infer actual arrival from
   map proximity. Replace synthetic coordinates when the shared simulation supplies
   a documented longitude/latitude contract.

Provider shape: incident_id, ambulance_id, zone, dispatched_at, updated_at,
status (assigned/approaching/arrived), arrived_at (NULL until confirmed),
route (numeric x=longitude/y=latitude in WGS84 degrees), progress (0–1), eta (currently NULL).
The module does not expose raw records, incident selectors or staff outputs.
Its host app must also enforce staff authorization on every staff server output.
A bearer code grants whoever possesses it access until expiry; this is a demo
access mechanism, not verified caller identity or production authentication.

Check: `Rscript app/caller/verify.R`.
No dependencies or changes to QC formulas. Staff/QC app files were preserved.

## Map dependencies and attribution

Leaflet 1.9.4 is vendored in www/vendor/leaflet with its BSD license. JavaScript
and stylesheet were checked against the official published SHA-256 integrity
hashes. Reference: https://leafletjs.com/examples/quick-start/ .

Street tiles: https://tile.openstreetmap.org/{z}/{x}/{y}.png . Visible attribution
credits OpenStreetMap contributors. Tiles require an internet connection; no tile
prefetch or offline map download is implemented. Browser HTTP caching is used;
the service worker does not cache location payloads or map tiles. Referrer policy
is strict-origin-when-cross-origin; private tracking fragments never enter HTTP
referrers. Before using real incident locations, review the map provider/privacy
arrangement; this preview uses synthetic coordinates only.
Tile policy: https://operations.osmfoundation.org/policies/tiles/ .

The caller layout groups status/ETA above the Queens map, includes a Recenter
button, and puts the last location update directly beneath the map. The house
means incident location (including incidents outside homes). Delayed location
updates show an explicit warning; confirmed arrival has a prominent time banner.

## Confirmed destination

The caller sees **Destination not yet confirmed** until a trusted provider supplies
`confirmed_destination` with `name`, `address`, and POSIXct `confirmed_at` via
`caller_confirm_destination`. Scene arrival never selects or confirms a hospital.
The panel displays the supplied confirmation timestamp and notes that the crew
may update the destination. It remains separate from the incident map.

The standalone caller preview has no hospital-selection or confirmation button.
A shared EMS provider must call `caller_confirm_destination` on the assigned
incident and persist that incident in the shared store. `caller_server` polls its
authorized lookup every second, so an open caller view receives the new destination
without a refresh. The current standalone preview remains unconfirmed until that
provider integration is wired. No real hospital catalog is bundled here.

The destination panel requires current incident authorization and clears with invalid access.
Status and connection messages remain in the response and map panels.
