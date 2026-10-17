# Combined role interfaces

The main app now integrates `branch-sub-supervisor`, `codex/caller-tracking`,
and the responder/map/transport work from main. Run `Rscript app/testme.R`
(default http://127.0.0.1:3838); set PORT=5300 for the existing preview.
Dependencies: shiny, openssl, jsonlite, ggplot2. Tests additionally need testthat.

- Supervisor: ordinary mode, and a developer mode whose top bar adds one synthetic completed call to the selected zone/priority per click, in `supervisor.R`.
- Responder: call replay, weather cards, road maps, hospital transport, in `responder-server.R`.
- Caller: password-free expiring incident links and tracking, in `caller/caller.R`.

Caller assets are namespaced under caller-assets; only one Leaflet copy loads.
Responder and Caller now share one process-local trip state. Supervisor shows
that same active trip and receives canonical scene/hospital completion updates.
The ETA branch is preserved, but validated ETA is not yet displayed in the UI.
No real EMS request is sent. Local data and account state reset with the process.

# Emergency Response Quality Monitor

Supervisor interface scaffold built from `demos/shinyapp/app.R`.

From the repository root, run `Rscript app/testme.R`, then open
http://127.0.0.1:3838. Requires R and the `shiny` and `openssl` packages.

Upload a CSV containing `incident_id`, `zone`, `call_priority`,
`dispatch_time`, and `on_scene_time`. The interface checks the column
names and previews records, with zone and priority filters and a CSV export.
Full record validation, date filtering, response calculations, control charts,
and investigation flags are pending H-03. Preview rows are not validated incidents.
Uploads remain in the current Shiny session; no database or live dispatch feed exists.

Baseline date controls capture proposed settings only. No statistical result is
displayed until the separate statistical engine is implemented and verified.

The welcome screen routes to Supervisor, Primary responder, or Person calling
for EMS. Supervisor opens the CSV dashboard; responder and caller views are
explicit prototype placeholders. Change role returns to the welcome screen.
Supervisor and Primary responder require their matching demo accounts. Caller needs no login. No EMS requests are sent.

Demo usernames are `supervisor.demo` and `responder.demo`. Disposable passwords
are generated in memory when the application starts and displayed in the login
dialog for hackathon reviewers. Password verification uses salted bcrypt-PBKDF.
These shared demo accounts are for synthetic data only; they are not production
authentication. Five failed attempts trigger a 30-second per-session delay.
Logging out reloads the page, ending staff access and clearing uploaded data.


## Installable web app (PWA)

The existing Shiny website now includes a web manifest, 192/512px app icons,
Apple touch icon, service worker, and a public offline fallback page.
Open the app once while online. Chrome/Edge offer installation when supported;
Safari uses Share > Add to Home Screen on iPhone/iPad or File > Add to Dock on Mac.
The interface also includes installation instructions and a conditional install button.

Deploy behind HTTPS (localhost is allowed for development). Relative manifest,
worker, and icon paths support deployment below a Posit Connect app URL.
The worker caches only its explicitly listed public assets. It does not cache
login responses, uploaded incidents, downloads, or Shiny session traffic.
Offline startup shows a connection-unavailable page; interactive features still
require the R/Shiny server and its WebSocket connection. This does not provide
background dispatch, offline incident entry, push notifications, or production auth.
The public demo credentials and account behavior are unchanged.

Reference: https://developer.mozilla.org/en-US/docs/Web/Progressive_web_apps/Guides/Making_PWAs_installable


## Agreed dispatch workflow (planned)

Dispatcher enters the caller's address → app identifies the incident zone →
filters available responders → ranks candidates by estimated travel time from
current responder locations → dispatcher confirms the assignment → assigned
responder receives the incident. Explicitly show when no responder is available.
The hackathon version will simulate addresses, responder locations, availability,
and travel estimates. This workflow is agreed but not implemented yet; it does
not currently select responders or send incident notifications.


## Responder incoming-call simulation

Log in as Primary responder using the displayed disposable demo account. Choose
Rain + rush hour, Dry + light traffic, or Snow/ice + road closure; then select
Simulate incoming call. Replay advances 30 simulated seconds per real second:
assigned at 0, en route at 30 seconds, approaching at 10 minutes, and automatically
on scene at 12 minutes. Pause/resume and reset are session-local controls. New
calls restart the deterministic sequence; scenario changes apply on the next call.

Traffic/weather prompts are authored coordination examples, not measured traffic,
weather, turn-by-turn routing, a prediction model, or proof of minutes saved.
Markers are fictional near Astoria Park. Movement follows a road-driving route
returned by the public OSRM service and stored in www/demo-road-route.json.
The snapshot uses synthetic endpoints and excludes live traffic, weather, closures,
and emergency-vehicle rules. It is not validated operational navigation. No GPS or address geocoding is used.
The basemap requires internet access and credits OpenStreetMap contributors.
Leaflet 1.9.4 is vendored in www/vendor with its license. Map source/license:
https://www.openstreetmap.org/copyright and https://leafletjs.com/.
This standalone UI replay is not yet wired to the H-09 dispatch feed or QC engine.

Weather icons are accessible inline SVGs for sun, rain, heavy rain, and snow/ice.
A Heavy rain + rush hour scenario is available. Road-route provenance and distance
are stored with the snapshot; no duration estimate or time saving is inferred.

Delay-reduction prompts appear as separate bordered checklist cards; ticks are
session-only and reset on a new demo call. Route distance is displayed in miles
(1 mile = 1609.344 meters); source geometry retains its native meter units.


## Zone outlines and hospital transport demo

The assignment briefing uses compact icon/label cards, retaining reported reason,
scene, priority, unit and access. Map controls switch between the active road route
and all five selected Queens zone groupings. Solid outlines have opacity 1 and
light interior fill. Boundaries come from NYC DCP 2020 NTAs (9nt8-h7nd):
Astoria aggregates QN0101/02/03/04 and QN0191; Flushing aggregates QN0704/05/07;
Jamaica uses QN1201; Forest Hills QN0602; Far Rockaway QN1401. These app-defined
statistical-area groupings are not official EMS districts or definitive neighborhood
boundaries. Source: https://data.cityofnewyork.us/resource/9nt8-h7nd.geojson

Hospital A/B/C are entirely fictional; positions and treatment capabilities are
invented for this demo. `demo-hospitals.json` documents their road distance from
the synthetic scene. A dispatcher/crew-provided scenario requirement filters
matching facilities and sorts them by road distance, without diagnosing symptoms
or selecting a destination automatically. No real capacity or diversion data exists.
Stroke care demonstrates a no-match result. Crew confirms a matching destination
after scene arrival (Advance to scene is a demo shortcut). The map switches to its
OSRM road snapshot; an 8-minute simulated replay follows, unrelated to predicted
travel time. At its end the responder manually confirms Arrived at hospital.
The arrival confirmation uses local wall-clock time, clearly labeled separately
from simulated elapsed time. It stays in this session and resets with a new call.
The original scene-response clock remains stopped; hospital transport is separate.
Requires the `jsonlite` R package in addition to shiny and openssl.

Supervisor integration preserves the collaborator dashboard. Responder UI logic
is isolated in app/responder-server.R, shared scenario helpers in responder-demo.R,
and styles in www/responder.css. Set PORT=5300 for the existing local preview.


## Shared palette and caller presentation

Reference palette: https://i.pinimg.com/736x/fe/1f/30/fe1f30a657adaf103eac7a4e23f263ff.jpg
Exact colors: navy #2C3E50, blue #AFC6D9, green #6B8F7A, red #D15B4A,
yellow #E6C35A, and cream #EFEAE2. Cream is the canvas, navy is the main text and
primary control color, and blue is the soft accent. Map colors remain distinct and labeled. Caller uses larger text,
softer cards, plain-language status, and expandable trip/map details. Private-link
access, expiry, incident isolation and crew-confirmed hospital rules are unchanged.

Change role clears the incident fragment before reloading, preventing a private
Caller link from reopening after logout. Deployment paths and query parameters
are retained; the new session starts at the role picker without prior access.


After confirming a hospital in the Responder demo, View caller tracking opens an
expiring incident-scoped link with that exact hospital name and fictional map
location. The Caller map uses the responder's scene route and shows scene arrival,
without restarting the standalone caller replay. Switching to Caller ends staff
access. The link does not send a message or EMS request; it is a demo handoff.


## Shared trip synchronization

One active synthetic Q-12 trip persists across role changes and browser sessions
in this R process. Each new call gets a unique incident ID. Caller links refer to
that trip, never create a second replay, and stop authorizing it after reset or
replacement. A confirmed hospital supplies a hospital map marker and legend item.
The ambulance follows the same road geometry and transport progress as Responder.
Supervisor ordinary mode shows the active trip and upserts one canonical record
at scene arrival, updating it on transport departure and manual hospital arrival.
No hospital completion is inferred merely from the marker reaching its destination.
Synthetic event timestamps use the demo clock; hospital confirmation also displays
local wall-clock time separately. Multiple open roles do not multiply replay speed.
Process restart clears this in-memory state; this is not a multi-worker backend.
Regression command: `Rscript tests/verify-shared-trip.R`.


The shared-incident merge consolidates canonical records and Caller lookup in
`app/shared-incidents.R`, preserving the collaborator's frozen assignment-time
ETA and full hospital route. `app/shared-trip.R` now holds shared responder UI
state only. The legacy responder-bridge helper is no longer wired into app.R.
Replay ticks continue in Caller mode, are deduplicated across sessions, and staff
role changes reuse the same claimed incident. Hospital arrival remains an explicit
crew confirmation. Both integration regression scripts cover the merged behavior.
