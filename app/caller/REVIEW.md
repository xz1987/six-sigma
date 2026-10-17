# H-16 verification

Implemented as an isolated caller module and executable PWA preview. Staff app
and local QC interface files were not overwritten. No commit or push performed.

| Check | Result |
|---|---|
| Random 256-bit access codes, incident binding, expiry | PASS |
| Invalid token/incident ID cannot retrieve records | PASS |
| Two incidents remain scoped to their respective tokens | PASS |
| Closing access clears rendered tracking | PASS |
| New sessions start unauthorized | PASS |
| Already-open access expires or is revoked | PASS |
| Assigned, approaching, confirmed arrival and reset | PASS |
| Map starts/ends at documented synthetic coordinates | PASS |
| No unsupported ETA presented | PASS |
| Standalone Shiny server initialization | PASS |
| Manifest and icon dimensions | PASS |
| Service-worker caching restricted to public assets | PASS (source inspection) |
| Safari authorized screen and confirmed arrival | PASS (observed) |
| Teal status and map card layout | PASS (Safari visual inspection) |
| Existing QC/comparison suite | FAIL 0 / WARN 0 / SKIP 0 / PASS 143 |
| Hosted HTTPS installation and offline recovery | PENDING |
| Shared dispatcher/EMS event integration | PENDING |
| Validated ETA model integration | PENDING H-11/H-12 |
| Geographic Queens basemap and ambulance/house markers | PASS; real OSM streets, synthetic positions |

Run `Rscript app/caller/verify.R`. Actual output:

    PASS: random expiring access codes; incident isolation; invalid/expired access denied; close and session isolation
    PASS: assigned/approaching/confirmed arrival; map endpoints; unavailable ETA; real module output checks
    PASS: standalone PWA server initializes; caller assigned, approaching, arrival and reset outputs
    PASS: expiry and revocation remove already-open incident outputs

The module lookup runs server-side and sends only one authorized incident. The
standalone demo creates process-shared grants at synthetic dispatch creation.
Merge must replace demo issuance with the shared dispatcher access workflow.
A bearer token demonstrates incident scoping, not caller identity verification.

## Direct tracking-link update

Caller password/code entry removed. A simulated dispatch automatically issues a
unique expiring private URL fragment. It can open the same incident across separate
browser sessions on one R server. Public incident IDs are not access credentials.
Server checks pass for dispatch issuance, cross-session link use, expiry, invalid
links, correct status output and absence of password fields.

Actual additional output:

    PASS: dispatch creates distinct private links; cross-session access; IDs denied; expired grants removed; no password field

Updated dispatch-creation screen loaded in Safari. End-to-end browser click-through
of the new link is still pending; browser interaction was interrupted. Original
status/map/arrival screen was visually verified before this update.

Integration still requires the shared dispatcher to call the issuance hook and
deliver the private link to the caller. No SMS/telephone integration, validated ETA
or actual GPS feed is implemented by this change.

## Queens map update

Replaced illustrative SVG grid with Leaflet/OpenStreetMap. Safari verified real
Astoria street tiles and ambulance/house character marker elements after opening
a dispatch-generated private link. Viewport remains persistent during updates.
All five neighborhood fixtures have coordinates inside the Queens demo envelope.
Map payloads are cleared when access is invalid; arrival places the ambulance at
the house location. Location movement is synthetic and not road routing or GPS.

    PASS: geographic Queens coordinates; JSON latitude/longitude; ambulance reaches house; unauthorized map cleared

Leaflet official integrity hashes checked; BSD license retained. No new R Leaflet
package is required; jsonlite serializes authorized map payloads.

## Caller layout refinement

Status and ETA precede the persistent Queens map; update time is directly below
it. Added Recenter control, incident-location house label, prominent confirmed
arrival banner with timestamp, and explicit delayed-location warning. Caller link
views hide the synthetic dispatch controls. Stale map marker has muted styling.

    PASS: delayed-location warning and stale marker state; Recenter and incident-location labels

## Resumed verification — 2026-10-16

Reran `Rscript app/caller/verify.R`: all seven PASS lines printed, exit 0.
Safari click-through verified: Create simulated dispatch generates the link;
Open caller tracking hides dispatch controls and shows the same synthetic Astoria
incident, Ambulance assigned, Estimate unavailable, and the Queens map with both
incident and ambulance markers. No password entry was required.

Shared dispatcher/EMS integration and hosted HTTPS installation remain pending.
The local staff app still has its separate caller view; it does not source this
module. No changes were made to staff-owned files or ETA calculations.

## Confirmed destination / next steps extension

Implemented explicit trusted-provider destination confirmation with facility name,
address and timestamp, independent of scene arrival. Missing confirmation remains
unavailable. The preview dispatch harness can confirm a clearly fictional facility.
What happens next follows assigned, approaching and arrived status.

`Rscript app/caller/verify.R` exits 0 with the previous seven PASS lines plus:

    PASS: explicit crew destination; arrival does not imply hospital; cross-incident isolation; unauthorized panels cleared; status-based next steps

Safari click-through and screenshot visually verified both new cards, fictional
name/address, confirmation time, and assigned-status next steps above the map.
Shared staff provider wiring remains pending; this change provides its hook.

## Simplified caller view

At the team's request, removed the What happens next card and its renderer.
ETA/status and map stay first, followed by the confirmed destination. Connection
and destination explanations remain in their respective panels.
`Rscript app/caller/verify.R`: eight PASS lines, exit 0.
