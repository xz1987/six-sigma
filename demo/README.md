# H-05: reproducible synthetic dataset demonstration

All data are invented. These results demonstrate software behavior, not Queens
EMS performance, clinical targets, or weather causation. Run each scenario
separately; stable and planted share IDs and must never be concatenated.

## Run the complete demonstration

From the repository root with base R installed (no extra packages required):

```sh
Rscript demo/run-datasets.R
```

The script calls the application's existing `app/qc.R` functions; it does not
implement a second QC formula or change any application files. It checks the
results below and stops with an error on a mismatch. Successful final output:

```text
PASS: stable and planted baseline limits are identical.
H-05 DATASET DEMO PASS
```

A captured complete run is in [expected-output.txt](expected-output.txt).

Input definitions, generation assumptions and the full invalid-row inventory:
[dataset codebook](../data/README.md). Baseline is January–December 2025;
monitoring is January 2026. Membership follows assignment timestamps, with
half-open date windows. Measurements are assignment-to-confirmed-scene minutes;
these CSVs do not contain hospital transport times.

## 1. Stable process

```sh
Rscript demo/run-datasets.R stable
```

Expected output:

```text
stable.csv: input=3465 accepted=3465 rejected=0 weather_normalized=0
baseline=3060 monitoring=405
Astoria/High: center=8.159166667 I_UCL=11.456815513 MR_UCL=4.050805462
Arithmetic exceedances (not approved alerts): Individuals=1 Moving_range=5
Individuals IDs: M-G05-0008
H-05 DATASET DEMO PASS
```

All 15 groups show `baseline requires review`. Read this as: no deterioration
was deliberately planted, but a stationary generating process can still produce
limit exceedances. Stable does not mean guaranteed zero signals or human approval.

## 2. Planted delay

```sh
Rscript demo/run-datasets.R planted
```

Expected output:

```text
planted_delay.csv: input=3465 accepted=3465 rejected=0 weather_normalized=0
baseline=3060 monitoring=405
Astoria/High: center=8.159166667 I_UCL=11.456815513 MR_UCL=4.050805462
Arithmetic exceedances (not approved alerts): Individuals=4 Moving_range=6
Individuals IDs: M-G01-0011, M-G01-0007, M-G01-0013, M-G05-0008
H-05 DATASET DEMO PASS
```

Read this as: three Astoria/High monitoring responses were changed to exactly
30 minutes. They exceed the unchanged 11.456815513-minute individuals upper
limit. Together with the original natural exceedance, there are four. Only one
additional moving-range exceedance appears: consecutive equal 30-minute values
have zero range between them. These events request investigation; they establish
no cause. Baseline estimates must not be recalculated from monitoring delays.

## 3. Edge cases

```sh
Rscript demo/run-datasets.R edge
```

Expected output includes:

```text
edge_cases.csv: input=270 accepted=261 rejected=9 weather_normalized=2
baseline=253 monitoring=8
Astoria/High monitoring: n=5 median=9.0 p90=9.6
Astoria/High assignment order: E-M-01, E-M-02, E-M-03, E-M-04, E-M-05
Unknown weather monitoring records: 3
H-05 DATASET DEMO PASS
```

Group statuses: one `baseline requires review`, one `degenerate baseline`
(Flushing/Medium constant at 10 minutes), and 13 `insufficient baseline data`.
Read this as: inadequate or constant baselines cannot produce validated alerts.
The nine rejected rows include both ambiguous duplicate copies, missing/invalid
required values and a negative duration. The script prints each issue.

Blank and invalid weather normalize to Unknown without rejecting otherwise valid
incidents. Explicit Unknown is already valid, so three monitoring records have
Unknown weather while only two values need normalization. Tied assignment times
are ordered by incident ID; arrival order does not control the QC sequence.
The median and type-7 p90 describe five valid Astoria/High responses, not all rows.

## Review gate and numerical checks

The normal `qc_analyze` path disables all monitoring alerts until an appropriate
baseline review is recorded. The script asserts that gate remains closed. The
full-scenario exceedance counts use the existing `imr_monitor` arithmetic
primitive solely to demonstrate numerical behavior; they are not approved
operational alerts and create no approval records. The edge scenario retains
its unavailable statuses. Eligibility thresholds do not prove suitability.

Limits use only the 12-month baseline. Moving ranges follow assignment order
within each neighborhood/priority, with the last baseline response preceding the
first monitoring response. Weather does not change limits. Relevant assumptions
include representative independent observations, baseline stability, distribution
shape and no unresolved seasonal structure; see [SPEC](../SPEC.md).

## Optional Shiny illustration

With the app dependencies installed, run `Rscript app/testme.R` and open
`http://127.0.0.1:3838`. Log in as Supervisor using the disposable credentials
shown by the app. Open Developer settings and explicitly enter Developer mode;
choose Stable process or Planted delay. Use its replay controls and reset before
switching scenarios. The synthetic baseline-review fixture is a demonstration
setting, not real approval.

The dashboard replays source January observations on its synthetic replay
calendar; filters and incomplete replay can yield subsets of the full-file
counts above. The console commands are the exact full-dataset reference. The
current UI offers only stable/planted presets; run edge cases through the
console command rather than claiming an unavailable upload or preset. Shared
caller/responder incidents are a separate demonstration:
[shared workflow](shared-incidents.md).

## Independent reproduction completed

Isabel Wilmot ran `Rscript demo/run-datasets.R` from the integration checkout
at commit `200d1f6` and supplied the complete terminal output in the session on
2026-10-16. The combined command runs all three scenarios.

- Stable matched: 3,465 accepted, 0 rejected; 1 individuals and 5 moving-range exceedances.
- Planted matched: 3,465 accepted, 0 rejected; 4 individuals and 6 moving-range exceedances.
- Edge matched: 261 accepted, 9 rejected, 2 weather values normalized;
  Astoria/High median 9.0 minutes and p90 9.6 minutes.
- Output confirmed identical stable/planted baseline limits and ended with
  `H-05 DATASET DEMO PASS`.

This fulfills the independent reproducibility check for H-05. It does not
constitute operational baseline approval or complete hosted-app verification.
