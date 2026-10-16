# DIIS synthetic ambulance-response fixtures and codebook

**All incidents, response times, priorities and weather are invented.** These
files contain no real dispatch, patient, ambulance, GPS or weather observations.
Neighborhood names are real; the numerical differences are not evidence about
Queens EMS performance. This completes the dataset/codebook scope of H-02,
not the production statistical functions, Shiny interface or replay feed.

## Files and purpose

| File | Rows | Purpose |
|---|---:|---|
| `stable.csv` | 3,465 | Stationary generating process, no planted deterioration; 3,060 baseline and 405 monitoring incidents. |
| `planted_delay.csv` | 3,465 | Same baseline and incident attributes; exactly three monitoring arrivals changed to produce 30-minute durations. |
| `edge_cases.csv` | 270 | Sparse/constant baselines, invalid completed records, tied assignment times, out-of-order completions, missing/invalid weather. |
| `generate.R` | — | Recreates all three CSVs using base R and seed 5300. |
| `validate.R` | — | Audits fixture structure, coverage and expected results; not the application's QC implementation. |

The two full datasets are alternative scenarios, not separate batches to combine.
They intentionally share IDs and baseline records. Load one scenario at a time;
combining them would create duplicates. The edge dataset is a separate scenario.

From the repository root, no additional R packages are needed:

```sh
Rscript data/generate.R
Rscript data/validate.R
```

The committed CSVs are the canonical fixtures. Regeneration uses R's default RNG
with a fixed seed (generated and checked with R 4.6.1); across R/RNG versions,
validate results and review changes rather than assuming byte-identical output.

## Observation, fields and units

One row is one synthetic **completed ambulance assignment** to an incident.
Version one assumes one responding assignment per incident; multiple units and
reassignments are not represented. CSVs use UTF-8, a header, quoted text fields,
no row-index column, and empty fields for deliberately missing values.

| Field | Type / units | Definition |
|---|---|---|
| `incident_id` | Text; no units | Unique incident identifier within a scenario, except intentional duplicate/missing-ID cases. Treat as text. |
| `zone` | Categorical | Incident neighborhood: `Astoria`, `Flushing`, `Jamaica`, `Forest Hills`, `Far Rockaway`. Not ambulance home zone. |
| `call_priority` | Ordered category | `High` (most urgent), `Medium` (urgent), `Low` (lower urgency). Synthetic labels, not official triage codes or clinical instructions. |
| `weather_condition` | Category | `Dry`, `Rain`, `Snow/Ice`, `Unknown` at incident location and assignment time. Descriptive context only. |
| `dispatch_time` | ISO 8601 timestamp with numeric UTC offset | Time a unit is assigned, despite the inherited dispatch column name. This starts the response clock. |
| `on_scene_time` | ISO 8601 timestamp with numeric UTC offset | Feed-simulated confirmation that the ambulance has stopped at the scene. This ends the response clock; not proximity or patient contact. |

Weather definitions: Dry means no rain or snow/ice; Snow/Ice takes precedence
for mixed winter conditions; Rain denotes rain otherwise. Unknown means
unavailable. Blank weather or unrecognized weather labels map to `Unknown`
with a reported issue and **do not exclude otherwise valid incidents**. In these
fixtures Snow/Ice is limited to November-March. No temperature, intensity or
actual weather-source information is supplied.

Derived `response_minutes` is the elapsed timestamp difference in seconds divided
by 60 after timezone-aware parsing. Do not subtract formatted local clock strings.
For charts, derive assignment hour and weekday in `America/New_York`; the numeric
offsets already encode daylight saving time. Durations are generated at integer
seconds and may have fractional minutes. Convert timestamps to UTC before sorting;
use incident ID to break assignment-time ties.

## Periods and group sizes

- Baseline: `2025-01-01T00:00:00-0500` inclusive to
  `2026-01-01T00:00:00-0500` exclusive, based on **assignment time**.
- Monitoring: January 2026, from `2026-01-01T00:00:00-0500` inclusive to
  `2026-02-01T00:00:00-0500` exclusive.
- These are historical synthetic dates. The replay advances simulated time;
  it must not pretend these are current real calls.
- The baseline spans 12 calendar months; this does not automatically remove
  seasonality or establish that I-MR assumptions hold.

Each cell below is baseline / monitoring count in either full dataset:

| Neighborhood | High | Medium | Low |
|---|---:|---:|---:|
| Astoria | 120 / 20 | 180 / 25 | 240 / 30 |
| Flushing | 132 / 21 | 192 / 26 | 252 / 31 |
| Jamaica | 144 / 22 | 204 / 27 | 264 / 32 |
| Forest Hills | 156 / 23 | 216 / 28 | 276 / 33 |
| Far Rockaway | 168 / 24 | 228 / 29 | 288 / 34 |

All 15 full-dataset groups exceed 100 baseline incidents, cover every baseline
month, and have nonzero variation. These checks establish **eligibility**, not
human baseline approval. No baseline is pre-approved by these files. Actual
monitoring flags must remain disabled until the SPEC review is documented.
Expected exceedances below are arithmetic checks assuming an approved baseline.

## How the fixtures were invented

`generate.R` uses independent normal draws, rounded to integer seconds, with
invented priority means of 480, 660 and 900 seconds (8, 11 and 15 minutes) and a
60-second generating standard deviation. Neighborhoods have the same generating
mean within a priority; observed differences arise from random sampling. None of
these values is a clinical target or a measured Queens parameter.

Baseline incidents are allocated across all 12 months before selecting random
days 1-28 and hours/minutes. Monitoring assignments fall in January 2026. DST
transition dates are avoided in generation; the timestamps still include both
winter and summer offsets. This is not a comprehensive DST test fixture.
Weather is sampled with nominal probabilities .65 Dry, .25 Rain, .08 Snow/Ice,
.02 Unknown; warm-month Snow/Ice draws become Rain. Weather does **not** change
generated response durations. The stable scenario deliberately omits rush-hour,
seasonal and weather effects so it can demonstrate a simple stationary model.
It is field-shaped test data, not a calibrated operational simulation.

Do not remove random long responses to force a signal-free baseline or chart.
A stationary generating process can still produce control-limit exceedances.
The file name `stable.csv` means no planted process change, not guaranteed
absence of signals or automatically approved chart assumptions.

## Expected full-scenario results

The audit uses the SPEC's baseline-only I-MR formulas. `stable.csv` and
`planted_delay.csv` have **identical** baseline estimates in every group.
For Astoria/High:

| Quantity | Expected value |
|---|---:|
| Baseline count | 120 |
| Baseline mean | 8.159166667 minutes |
| Mean moving range | 1.239915966 minutes |
| Individuals upper limit | 11.456815513 minutes |
| Moving-range upper limit | 4.050805462 minutes |

Use approximately 1e-8 absolute tolerance for the rounded values shown here;
production H-03 tests should compare independently calculated unrounded values.

Planted incidents, in chronological assignment order:
`M-G01-0011`, `M-G01-0007`, `M-G01-0013`. Each is Astoria/High and has a duration
of exactly **30 minutes** in `planted_delay.csv`. Only these three arrival
timestamps differ from `stable.csv`. The first creates an additional MR signal;
the next two have equal 30-minute durations and thus zero moving range between
them. The planted delay is not assigned a real-world cause.

| Monitoring upper-limit exceedances | Stable | Planted delay |
|---|---:|---:|
| Individuals | 1 | 4 |
| Moving range | 5 | 6 |

The naturally occurring individuals exceedance is `M-G05-0008`.
The stable MR exceedances are `M-G11-0013`, `M-G11-0003`, `M-G15-0022`,
`M-G12-0017`, and `M-G01-0001` (list order is not chronological).
The planted scenario adds the three named individuals exceedances and MR
exceedance `M-G01-0011`. These counts were independently cross-checked in Python.
Do not describe all chart signals as planted events or proven process changes.

The first completed monitoring response in each group compares to the final
baseline response in that same group. This boundary range is monitored but is
never included in baseline estimation. All later ranges follow assignment order,
not arrival order. Unknown weather remains part of the same QC sequence.

## Edge-case inventory and expected handling

`edge_cases.csv` contains 253 valid baseline and 8 valid monitoring records,
plus 9 deliberately invalid records: **261 accepted, 9 rejected**.

| Group | Baseline count | Expected baseline handling |
|---|---:|---|
| Astoria / High | 120, all 12 months | Eligible for documented review; copied from full baseline. |
| Flushing / Medium | 120, all 12 months | Constant 10-minute durations; `degenerate baseline`, flags disabled. |
| Jamaica / Low | 12, all 12 months | Below count threshold; `insufficient baseline data`. |
| Far Rockaway / High | 1 | Below count and month-coverage thresholds. |
| Other 11 groups | 0 | Empty baseline groups; insufficient data, not all-clear. |

Valid monitor IDs `E-M-01` through `E-M-08` include:

- `E-M-01` and `E-M-02`: identical assignment timestamps, distinct IDs and
  arrival times. Retain both and order by ID for the tie.
- `E-M-02`: blank weather. Retain; normalize to Unknown.
- `E-M-03`: weather `Hail`, intentionally outside this schema. Report and
  normalize to Unknown; retain the incident.
- `E-M-04`: explicit Unknown weather. Retain; this is an allowed category,
  not an invalid-label issue. It completes before `E-M-03` despite being
  assigned later, exercising out-of-order completion handling.
- The five Astoria/High monitoring durations are 8, 9, 10, 8, 9 minutes in
  assignment order: median **9**, type-7 90th percentile **9.6** minutes.
- The other three monitoring records belong to degenerate/sparse groups.
  Valid duration does not make a baseline adequate.

Two weather values need normalization; three monitoring records have Unknown
weather after normalization. Retain them in response statistics, counts and
weather plots. Some weather categories and hour/weekday cells are sparse or
empty; display counts and no-data cells rather than zero-duration estimates.

| Deliberately invalid ID | Problem | Expected handling |
|---|---|---|
| `E-BAD-DUP` (two rows) | Duplicate ID | Reject both ambiguous copies and report two rejected rows. |
| `E-BAD-NEGATIVE` | Arrival before assignment | Reject. |
| `E-BAD-DISPATCH` | Missing assignment | Reject completed-record upload. |
| `E-BAD-ARRIVAL` | Missing arrival | Reject as an incomplete row in this completed-record CSV. |
| `E-BAD-TIMESTAMP` | Unparseable assignment | Reject. |
| `E-BAD-ZONE` | Brooklyn, outside selected zones | Reject. |
| `E-BAD-PRIORITY` | Critical, outside selected priorities | Reject. |
| Empty ID | Missing identifier | Reject. |

The missing-arrival CSV row is deliberately invalid **in completed-record upload
mode**. A legitimate dispatched feed event without an arrival is instead pending,
not rejected; arrival is supplied later by the feed. Do not replay invalid rows
as legitimate operational incidents.

## Replay contract for H-09

The feed is not implemented in H-02. These files supply its repeatable inputs:

1. Load baseline records and conduct the required review separately. Replay
   only valid **monitoring** records from one selected scenario.
2. Derive one assignment event and one automatic on-scene event per valid
   monitoring record. IDs can be `<incident_id>:dispatched` and
   `<incident_id>:on_scene`; repeated delivery must be idempotent.
3. Event payloads carry event ID, incident ID, type, timestamp, neighborhood,
   priority and assignment-time weather. Sort deliveries by absolute event time,
   assignment before arrival for ties, then incident ID.
4. An optional approaching event may be synthesized 30 seconds before arrival
   only when the duration exceeds 30 seconds. It represents a fictional
   500-foot proximity state, not a GPS/routing calculation; the response clock
   continues. No additional CSV column is required.
5. Start at the first monitoring assignment, use a documented configurable replay
   speed, and freeze simulated elapsed time while paused. Reset clears monitoring
   events and active calls and restores the same sequence and frozen baseline.
6. At the end of a clean full-file replay, 405 incidents have completed and no
   active incidents remain. Active counts during replay can be checked against
   the number assigned minus the number arrived by simulated time.
7. Arrival order can differ from assignment order. Recompute affected monitoring
   ranges over completed records in assignment order. Do not mix active elapsed
   time into completed-response statistics or update baseline limits.

## What collecting equivalent real data would require

An authorized dispatch/CAD export would need a stable incident/assignment ID,
incident location linked to defined neighborhood boundaries, an agreed mapping
from agency priority codes, and consistent unit-assignment and crew-confirmed
on-scene timestamps with offsets. Reassignments, cancellations and multiple units
would need an explicit observation policy before integration.

Weather context would require an identified observational source, location/time
matching, category rules and missingness reporting. No such service is connected
here. Avoid treating this demo's synthetic labels as real weather records.
Collecting actual records requires agency permission and appropriate handling of
sensitive operational information; this project includes no names, addresses,
patient details or real unit identifiers. Real data also require fresh baseline
suitability review; simulation results do not validate a real EMS deployment.
