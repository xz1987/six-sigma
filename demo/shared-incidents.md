# Shared synthetic incident and ETA walkthrough (H-23)

Run `Rscript app/testme.R` from this checkout. Dependencies are Shiny, openssl,
jsonlite and ggplot2; integration tests also need testthat. Use two tabs of the
same server process. This in-memory demonstration resets on server restart and
is not a multi-worker deployment or operational dispatch system.

1. Open Caller, choose Create demo tracking link, then Open caller tracking.
   Open Trip details to identify the incident. Before responder acceptance the
   Caller shows Waiting for assignment. After acceptance it shows the initial
   scene-arrival clock estimate and duration when available; it does not display
   the model's full 95% range. No caller login or code entry is needed.
2. In another tab open Primary responder and use its displayed disposable demo
   credentials. In Caller incident to receive select that same ID, then
   Accept assignment & begin response. A previously assigned incident cannot be taken by another session.
   Alternatively create a new incident from the responder and use its private
   caller tracking link. A new incident has a new ID and prediction.
3. The responder replay advances 30 simulated seconds per real second. Caller
   map movement and assigned/approaching/confirmed-scene status follow that same
   record. Pause freezes progress; Advance to scene is an explicitly synthetic
   shortcut. Scene arrival is assignment +12 simulated minutes, independent of
   the model estimate. The assignment-time prediction never changes on arrival.
4. After scene arrival choose a matching real hospital from the sourced directory, then Confirm & begin
   trip. The already-open caller tab displays the crew-confirmed destination. Shared-store writes notify caller sessions immediately; no page refresh or new link is required. Selecting an option alone does not confirm it; press Confirm & begin trip and look for the success message.
   Names, published capabilities and addresses are sourced; stored road routes
   use approximate facility locations. Hospital availability is not live or
   verified. Scene arrival alone must not display a destination.
5. After the 8-minute accelerated transport replay, select Confirm hospital arrival.
   A supervisor session in ordinary mode receives one canonical incident record
   with a 12-minute call-to-scene value (receipt and assignment coincide in this
   demo) and an 8-minute transport-to-hospital value. Choose today's period,
   the incident's actual zone and priority. Simulated completion times may be ahead of the
   wall clock; wait/use the appropriate displayed period around midnight.
   No baseline is re-estimated by these updates. Baseline review and QC eligibility
   gates still apply; synthetic ETA does not approve a QC baseline.
6. Reset revokes the old caller grant and clears the responder's session. Completed
   canonical observations remain available to the supervisor; they are not deleted
   to reset a display. A fresh call creates a fresh incident. Caller links expire
   30 wall-clock minutes after creation. Restart invalidates all links and records.

ETA is the unchanged H-11 validated synthetic weather model, fitted/checked once
at server startup using the frozen versioned fixture and acceptance gates. Scenario
weather, intensity, sustained wind (10 mph), gust (16 mph) and observation time
are explicitly synthetic assignment-time inputs. Traffic is not a predictor.
Missing/unsupported inputs produce unavailable; no wind values are imputed.
No actual arrival time is supplied to the assignment prediction. Predictions
remain labeled experimental/synthetic and distinct from actual elapsed time.
These are fixed assignment-to-scene estimates, not continuously updated remaining
travel times or hospital ETA. Model or fixture load failure yields unavailable.

Verification:

```sh
Rscript tests/test-shared-incidents.R
Rscript app/caller/verify.R
Rscript app/smoke.R
Rscript -e "testthat::test_dir('tests'); testthat::test_dir('tests/testthat')"
```

Checks exercise separate caller/responder server sessions against one incident
service, adoption without replacement, assignment exclusivity, fixed predictions,
missing weather, invalid/expired/revoked tokens, destination isolation, completed
metric emission without duplicates, and staff gating. Caller data and link tokens
never enter supervisor records or the service-worker cache.

## Caller hospital route

After **Confirm & begin trip**, the same caller link automatically receives the
confirmed hospital name/location, a 🏥 hospital marker and a blue stored hospital route.
The 🏠 house marker is shown during scene response and hidden during the hospital trip. The map fits the hospital route once;
subsequent replay ticks move the ambulance without resetting the caller's zoom.
Recenter includes the route and hospital. Every route is a stored OSRM/OpenStreetMap
snapshot to an approximate real facility location, not live navigation or a hospital ETA.

Transport advances 30 simulated seconds per second along the shared route. The
scene-response ETA remains fixed and refers to scene arrival only. At the end of
the 8-minute replay, the responder confirms hospital arrival. Missing route data
shows route unavailable; no straight-line route is substituted. Expired, invalid
or revoked caller access clears the hospital, route and incident together.

Use the newest preview port provided in chat; older running previews retain their
original code. Only the confirmed hospital's route is included in that caller's
server-scoped map output. Other incidents and candidate destinations are excluded.
