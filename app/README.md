# DIIS developer and function reference

Start with the [project README](../README.md) for setup, role walkthrough and
scope. This is an R/Shiny application, not a public REST API or an installed R
package. Source functions from their files for scripted analysis.

## Runtime and architecture

Run `Rscript app/testme.R` from the repository root. `PORT` defaults to 3838 and
`HOST` to 127.0.0.1. Runtime packages: shiny, openssl, jsonlite and ggplot2;
testthat is used by tests, rsconnect by publishing. Disposable staff passwords
are generated at startup and shown by the login dialog; do not replace them with
real credentials in committed files.

| File | Responsibility |
| --- | --- |
| `app.R` | Role selection, demo authentication, shared service and application startup |
| `shared-incidents.R`, `shared-trip.R` | Process-local incident lifecycle, tracking links and milestone publication |
| `responder-server.R`, `responder-demo.R` | Responder workflow, replay and hospital confirmation |
| `caller/caller.R` | Link-based Caller view and shared-state tracking |
| `response-metrics.R` | Canonical timestamp adapter; no QC formulas |
| `qc.R` | Validation, fixed I-MR limits, gated monitoring and descriptive statistics |
| `supervisor.R`, `supervisor-analytics.R` | Supervisor UI, isolated simulation, SPC patterns and historical summaries |
| `eta.R` | Independent synthetic ETA fitting, validation and prediction |
| `www/`, `caller/www/` | Maps, styles, browser behavior and offline fallback |

## Measurement inputs

Ordinary mode consumes shared records; users do not upload a CSV. Each incident
has the fields below. A metric is available only after its own start and end
exist. Timestamp strings require ISO 8601 seconds with `Z` or an explicit UTC
offset. Durations are elapsed minutes; local calendar displays use
`America/New_York`.

| Field | Meaning / allowed values |
| --- | --- |
| `incident_id` | Unique incident identifier |
| `zone` | Astoria, Flushing, Jamaica, Forest Hills or Far Rockaway; incident location |
| `call_priority` | High, Medium or Low; synthetic urgency categories |
| `weather_condition` | Dry, Rain, Snow/Ice or Unknown; invalid/missing becomes Unknown |
| `call_received_time` | Start of call-to-scene duration |
| `scene_arrival_time` | Confirmed scene arrival; end of call-to-scene duration |
| `transport_departure_time` | Start of hospital transport duration |
| `hospital_arrival_time` | Upstream confirmed hospital arrival; end of transport duration |
| `responder_id`, `response_system_source` | Optional identifying/provenance context |

Missing/invalid required values, ambiguous duplicate IDs and negative durations
are reported and excluded from completed QC. Missing weather is reported without
excluding an otherwise valid incident. Pending pairs do not become zero-minute
observations. The legacy fixture fields `dispatch_time`/`on_scene_time` are
assignment/arrival timestamps, not the full canonical contract. Synthetic adapters
create explicitly labeled proxies; production integrations must supply actual
milestones. See [codebook](../data/README.md) and [SPEC](../SPEC.md).

## QC function reference (`qc.R`)

All arguments below are R objects, with durations in minutes. These functions
return values rather than modifying shared trip state.

| Function | Parameters and result |
| --- | --- |
| `qc_parse_time(x)` | Timestamp vector or POSIXt; returns UTC POSIXct, invalid values as NA. |
| `qc_validate(records, mode=c('completed','active'))` | Data frame with legacy QC fields above. Returns completed, pending, rejected, issues and counts. Active mode retains missing arrivals as pending; completed mode rejects them. |
| `imr_fit(baseline)` | Finite nonnegative numeric duration vector in chronological order. Returns n, center, moving ranges, sigma, I/MR limits, last observation and computability status. Short/constant inputs have unavailable limits. This does not grant review approval. |
| `imr_monitor(fit, monitoring, enabled=TRUE)` | Fit plus ordered finite nonnegative monitoring vector. Returns durations, adjacent ranges and upper-limit exceedance fields. Disabled/unusable fit yields unavailable signals. Arithmetic primitive for tests; application integrations should use `qc_analyze`. |
| `qc_summary(x)` | Finite numeric vector; returns n, median, type-7 p90. Empty input returns n=0 and NA statistics. |
| `qc_box(x)` | Finite vector; returns quartiles, median, 1.5-IQR whiskers, outliers and points. Empty input returns unavailable summaries. |
| `qc_boxes(completed, group_col)` | Validated completed data and grouping-column name; returns grouped box summaries. |
| `qc_heatmap(completed, tz='America/New_York')` | Validated completed records; returns local hour/weekday median and count cells, preserving empty cells as missing. |
| `qc_period(baseline_start, baseline_end, monitoring_start=baseline_end, monitoring_end)` | Offset-bearing timestamps; requires 12 complete New York baseline calendar months and nonoverlapping monitoring. Returns validated half-open period boundaries and baseline month labels. |
| `qc_order(d)` | Orders a QC data frame by start (`dispatch_time`), then incident ID for ties. |
| `qc_baseline_key(baseline, period, zone, priority)` | Creates an identity for the exact reference observations/settings used to match a review. |
| `qc_diagnostics(b, fit, months)` | Ordered baseline records, fit and expected month labels; returns preliminary chart, monthly, distribution and lag diagnostics for review. |
| `qc_review(group, reviewer, rationale, decision=c('approved','unsuitable'), reviewed_at)` | Reviewed analysis group plus documented reviewer, rationale, decision and timestamp; returns a review tied to its baseline identity. Does not bypass insufficient/degenerate safeguards. |
| `qc_analyze(records, period, reviews=list(), mode=c('completed','active'))` | QC-shaped records, validated period and optional review records. Returns validation and 15 zone/priority groups with diagnostics, limits, summaries, status and monitoring evidence. Without suitable approval, operational monitoring flags are disabled. |

Minimal standalone example, independent of Shiny:

```r
source('app/qc.R')
x <- c(8, 9, 8, 7, 8)
fit <- imr_fit(x)
stopifnot(fit$center == 8, fit$mr_center == 1)
qc_summary(x) # n = 5, median = 8, p90 = 8.6
imr_monitor(fit, c(8, 30)) # arithmetic check, not baseline approval
```

## Metric adapter (`response-metrics.R`)

`response_metric_catalog()` lists the two metric IDs, labels and timestamp pairs.
`response_system_fields()` lists canonical fields;
`response_metric_empty_records()` creates an empty stream.
`response_metric_info(metric_id)` looks up one supported metric and rejects
unknown IDs. Choose `call_to_scene` or `transport_to_hospital`.

`response_metric_to_qc(records, metric_id, allow_mock=TRUE)` maps the selected
canonical timestamp pair onto QC's `dispatch_time`/`on_scene_time` columns;
`allow_mock=FALSE` prevents synthetic legacy fallback. Use that setting for real
integrations. `response_metric_completion_events(records, allow_mock=FALSE)`
returns metric completion events with IDs, metric, zone, priority, timestamp and
duration. `response_metric_connection_status(records)` describes which input
contract is present. Scene and transport remain separate observations and limits.

## ETA functions (`eta.R`)

The ETA target is legacy assignment-to-scene minutes, distinct from the final
call-received-to-scene QC definition. Inputs include zone, priority,
`dispatch_time`, weather condition, precipitation intensity (`None`, `Light`,
`Moderate`, `Heavy`), sustained `wind_speed_mph`, `wind_gust_mph` and
`weather_observed_at`. Training also needs `on_scene_time`. Gust must be at least
sustained speed; weather observation must precede assignment by less than one
hour. [ETA codebook](../data/eta/README.md) defines generation and partitions.

| Function | Parameters and result |
| --- | --- |
| `eta_prepare(d)` | Data frame; returns predictors, times, target and per-row unavailability reasons. |
| `eta_fit(d, weather=TRUE)` | Fits a log-duration candidate with weather terms, or the comparison model without those terms. Requires eligible sample/support and nondegenerate full-rank fit; returns candidate or unavailable status/reason. |
| `eta_validate(d)` | Runs fixed chronological train/validation/test checks against the simpler model, including predictive error and interval coverage; returns a frozen validated-synthetic bundle or unavailable result. |
| `eta_predict(bundle, d)` | Bundle plus assignment-time rows; returns status/reason, median/mean minutes, 95% individual prediction limits and arrival timestamps. Unsupported/incomplete input returns NA estimates, never zero. |

The prediction is frozen at assignment; advancing replay does not refit it.
Hospital route progress does not turn this model into a hospital ETA. Model
intervals describe uncertainty for a synthetic individual duration, not a
confidence interval for a real EMS service promise. `Rscript app/eta-view.R`
runs the reproducible ETA demonstration.

## Supervisor controls and chart interpretation

Active tracking selects metric, date window, zone and priority. Patterns are
descriptive. History analysis has independent metric/zone/priority, phase,
period and aggregation filters. Daily/weekly subgroup limits depend on n;
today's open New York day is excluded from closed subgroup history. Configured
priority service goals and optional zone overrides do not modify control limits.
Charts have visible legends and expandable **How to read this chart** buttons.

**Try demo mode** isolates generated observations. Choose Normal day or Day with
delays, then Start demo day. Next completed case reveals one of 12 hourly cases
(08:00–19:00 on the current New York date); Complete demo day reveals all 12.
The delayed scenario sets the final three selected-metric durations to 30 minutes.
The day-so-far average is the arithmetic mean of revealed durations. Replay time
is synthetic, including any displayed future time. Advanced settings retain
single-case and event controls. The connected live trip stays visible but its
records never enter generated graphs. Exit discards the generated records.

## Publishing and limitations

From the repository root with rsconnect installed, run:

```sh
Rscript app/manifestme.R
Rscript tests/verify-deploy-bundle.R
```

The manifest script bundles the required synthetic CSVs under `app/data/` and
writes `app/manifest.json`. Refresh it after runtime source/assets change. The
bundle check verifies startup without the parent repository. Publish the `app/`
directory to the course Posit Connect server using the team's deployment setup.
Keep publishing credentials in environment/repository secrets. Deployment details
and the verified hosted link remain the deployment owner's responsibility.
Configure one R process so separate role sessions share state. Records disappear
on restart; durable storage, operational authentication and multiworker shared
storage are future work.

The installable PWA requires HTTPS (localhost is exempt) and a server connection
for interaction. Its offline fallback is not offline dispatch. Browser map tiles
require internet. Hospital capabilities have dated sources; route snapshots and
vehicle movement do not include live traffic, emergency driving rules or GPS.
Vendored Leaflet licensing and route/source notes are retained in `www/` and the
[hospital codebook](../data/hospitals/README.md). Demo interactions send no EMS
request and do not provide clinical or operational navigation instructions.

## Reproduction checks

See the [root README](../README.md#verify-and-extend) for commands and
[tests/README.md](../tests/README.md) for existing numerical checks. Dataset
expected output is in [demo/README.md](../demo/README.md). A fresh-clone teammate
walkthrough and public hosted-link verification are still required for H-07;
local automated checks alone do not establish either.

## Final local audit — 2026-10-17

Audited pushed commit `8390b46`. Both test suites passed all 362 assertions:
QC 88, shared incidents 1, chart guides 67, daily demo 18, Supervisor 49,
ETA 100 and feed 39. Separate checks passed for independent Caller/Responder
sessions, caller-first assignment, role-stage guards, role synchronization,
phase-aware Caller ETA, shared trips, five-zone routes and the nine-hospital
directory. Caller verification, smoke, dataset demo, ETA demonstration,
isolated Supervisor day-demo verification and standalone deployment-bundle
startup also passed. Manifest file checksums matched the bundle.

A public, credential-free clone of this commit succeeded. Its smoke and dataset
demonstration passed using the installed R dependencies; this does not test
package installation on a new machine or replace a teammate's walkthrough.

Safari checks confirmed Caller waiting before assignment, updates after a
separate Responder tab accepts that incident, scene completion appearing in
Supervisor, and automatic Caller hospital destination, hospital-marker legend
and road-route updates after responder confirmation. Supervisor checks confirmed
expandable chart-guide buttons, a 12-case delayed day with three 30-minute
signals, the separate live-trip panel, and rendered full-year history charts
with current-day observations excluded from closed subgroups. Full completion,
reset and access revocation were verified by the automated session checks.

Known remaining items:

- The Supervisor day-demo module check emits 12 R warnings that `tzone`
  attributes are inconsistent. Its chronology, fixed-limit and isolation checks
  pass; the run is not warning-free.
- A hosted URL is not recorded in the current README. Public-host accessibility,
  deployed cross-session behavior and device installation still need verification.
- SPEC records the team's challenge summary. `docs/prompts.md` links to Devpost
  for the exact prompt and describes a web frontend plus a package/library or
  API; SPEC and CLAUDE permit the Shiny dashboard with sourced R functions.
  Reconcile the required delivery format with the organizer's current wording
  before claiming complete submission compliance. No packaged R library or
  public statistical API is implemented by this app.
- Add the verified live URL, complete the independent fresh-clone walkthrough,
  reconcile stale ledger entries and submit the repository/demo on Devpost.

The implemented incident-level QC and synthetic scenarios address the recorded
problem of variation and extreme delays hidden by averages. They demonstrate
software behavior; they do not establish reduced real response times, lives
saved, causal weather effects or operational deployment readiness.
