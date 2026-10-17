# DIIS Emergency Response Monitor

The DIIS Emergency Response Monitor is a Shiny prototype for reviewing
incident-level ambulance response durations across five selected Queens zones.
It pairs a shared synthetic caller/responder workflow with supervisor control
charts, so unusually long responses remain visible instead of disappearing in
monthly averages.

**Demonstration only.** It does not connect to 911 or EMS, dispatch a unit,
provide clinical advice, or offer operational navigation. All demo events and
response records are synthetic. For a real emergency in the United States,
call 911.

For a plain-language overview, open [About the app](../docs/about.md). See the
[synthetic dataset walkthrough](../demo/README.md),
[dataset codebook](../data/README.md), and [project specification](../SPEC.md)
for the analysis assumptions and known limitations.

## Run locally

From the repository root, install the runtime packages once, then start the app:

```sh
Rscript -e 'install.packages(c("shiny", "openssl", "jsonlite", "ggplot2"))'
Rscript app/testme.R
```

Open <http://127.0.0.1:3838>. Set `PORT=5300` to use port 5300. Staff demo
usernames and disposable passwords are displayed in their login dialogs. The
passwords are generated in memory each time the R process starts.

## Use the three role views

- **Caller:** Create a demo tracking link. It belongs to one synthetic incident,
  expires after 30 minutes, and does not send a message or call.
- **Primary responder:** Accept the queued synthetic incident, advance through
  the response steps, review the scenario-matched hospital list, confirm a
  destination, and confirm hospital arrival. Traffic and weather prompts are
  coordination examples; road routes are stored snapshots.
- **Supervisor:** Review scene and hospital durations, grouped by incident zone
  and simulated priority. Individuals and Moving Range charts use separate
  fixed baselines for each metric/group. The bundled stable and planted-delay
  fixtures can be replayed in Developer mode for a statistical demonstration.

The caller, responder, and ordinary supervisor views share the same incident
and confirmed milestones in one R process. The hosted demo uses one server
process to keep sessions together. State is in memory and clears when that
process restarts. The caller link is private to its incident and is revoked on
reset or replacement.

## Measurements and input contract

The supervisor keeps these durations separate:

1. `call_received_time` to `scene_arrival_time` (`call_to_scene_minutes`).
2. `transport_departure_time` to `hospital_arrival_time`
   (`transport_to_hospital_minutes`).

Each duration is computed in minutes from timestamps parsed as instants and
compared only with its own zone/priority baseline. Completed records require a
valid incident ID, one of the five zone names, a simulated priority, and both
timestamps for the selected metric. Negative/unparseable durations and
duplicate IDs are reported and excluded from that metric. Incomplete metric
pairs stay pending. Synthetic data require human review before monitoring
signals are enabled; an ineligible or degenerate baseline cannot produce
validated signals.

The application functions accept the following canonical response fields:

| Field | Meaning |
|---|---|
| `incident_id` | Unique incident key. |
| `zone` | `Astoria`, `Flushing`, `Jamaica`, `Forest Hills`, or `Far Rockaway`. |
| `call_priority` | Synthetic `High`, `Medium`, or `Low` category. |
| `weather_condition` | Assignment-time `Dry`, `Rain`, `Snow/Ice`, or `Unknown`; descriptive context only. |
| `call_received_time` | Call-received timestamp, including timezone/UTC offset. |
| `scene_arrival_time` | Confirmed scene-arrival timestamp. |
| `transport_departure_time` | Start of the hospital-transport duration. |
| `hospital_arrival_time` | Confirmed hospital-arrival timestamp. |
| `responder_id` | Optional descriptive unit identifier. |

`response_metric_empty_records()` creates the empty canonical data frame.
`response_metric_catalog()` describes the two metrics. `response_metric_to_qc(records,
metric_id, allow_mock=TRUE)` maps canonical timestamps to the QC function's
legacy start/end columns; `metric_id` is `call_to_scene` or
`transport_to_hospital`. With `allow_mock=TRUE`, legacy fixture rows can be
converted using deterministic synthetic timestamps. For real canonical input,
use `allow_mock=FALSE`. `response_metric_completion_events()` creates
milestone records for the supervisor feed. The current UI does not ingest a
live response feed or accept CSV uploads.

Synthetic fixture columns, generation, and units are documented in
[`data/README.md`](../data/README.md). Do not join `stable.csv` and
`planted_delay.csv`: their IDs intentionally overlap.

## Core R functions

- `qc_validate(records, mode)` checks required values and reports invalid rows.
- `imr_fit(baseline)` computes the Individuals and Moving Range center and
  control limits from baseline observations.
- `imr_monitor(fit, monitoring, enabled)` applies frozen limits to monitoring
  observations and marks limits disabled when review/eligibility requirements
  are not met.
- `qc_analyze(records, period, reviews, mode)` runs validation, baseline
  eligibility, review, analysis, and monitoring for zone/priority groups.
- `qc_summary(x)`, `qc_box(x)`, `qc_boxes(completed, group_col)`, and
  `qc_heatmap(completed, tz)` provide descriptive summaries and chart data;
  none of these add a separate investigation rule.
- `supervisor_signal_rules()` evaluates the documented supplemental SPC rules.
  `supervisor_group_signals()` groups signals for the dashboard. The main
  investigation rule remains an Individuals observation above its fixed UCL.
- `shared_create()`, `shared_claim()`, `shared_advance()`, `shared_transport()`,
  `shared_transport_advance()`, and `shared_hospital_arrival()` create and
  update one shared synthetic incident and its confirmed milestones.
- `caller_issue_access()`, `caller_lookup()`, and `caller_map()` issue an
  expiring incident-scoped demo grant, validate it, and describe the caller's
  map state. The Shiny module entry points are `caller_ui()` and
  `caller_server()`.
- `responder_server()` and `supervisor_ui()` / `supervisor_server()` implement
  the staff workspaces and connect them to the shared incident/QC functions.

Function arguments are ordinary R data frames, vectors, IDs, and settings as
indicated in each function signature. There is no public REST endpoint.

## Project layout

| Path | Purpose |
|---|---|
| `app.R` | Shiny entry point, role selection, ephemeral staff login, and shared services. |
| `qc.R` | Validation, I-MR analysis, limits, summaries, and chart data. |
| `response-metrics.R` | Canonical metric definitions and adapter to QC inputs. |
| `feed.R`, `workflow.R` | Deterministic synthetic event replay and supervisor analysis assembly. |
| `supervisor.R`, `supervisor-analytics.R` | Supervisor UI and supporting signal/summary calculations. |
| `responder-server.R`, `responder-demo.R` | Responder workflow and synthetic scenario cards. |
| `shared-incidents.R`, `shared-trip.R` | Process-local shared incidents, milestones, and responder UI state. |
| `caller/caller.R` | Caller tracking access, route state, and Shiny module. |
| `www/` | CSS, maps, route snapshots, icons, PWA assets, and the static About page. |
| `data/` | Bundled copies of the synthetic fixtures needed by the hosted app. |

The demo map uses bundled road geometry; it is not turn-by-turn routing and
does not account for live traffic, closures, or emergency vehicle rules.
Hospital services are transcribed from published sources listed in
`www/hospital-directory.json`; the app does not check real-time capacity or
availability. Statistical signals identify records for review and do not prove
cause or staff performance.

## Deployment

The current public demo is [DIIS Emergency Response Monitor](https://connect.systems-apps.com/content/063ac41c-40a1-49bf-88dd-1ed5497a367f/).
It runs on Posit Connect from this repository's `main` branch and the `app/`
manifest. From the repository root, run `Rscript app/manifestme.R` to copy the
required synthetic fixtures into the app bundle and write `app/manifest.json`.
Import the `main` branch in Posit Connect with manifest directory `app`.
Configure one process so caller, responder, and supervisor sessions share the
same in-memory incident. Demo incidents reset on server restart; bundled files
do not. Map tiles require internet access.
