# SPEC: Emergency Response Quality Monitor

## 1. Team and prompt
- **Team name:** DIIS.
- **Track:** On-campus (24 hours).
- **Prompt we chose:** Health Systems — Emergency Response (team-provided title). Confirm official prompt number and exact title for the selected track.
- **Challenge summary (provided by the team):** Ambulance dispatch-to-on-scene times vary across city zones; monthly averages hide extreme delays and patterns requiring investigation.
- **Project repository:** https://github.com/xz1987/six-sigma (current origin; public accessibility must be verified under H-08).

## 2. User and use case
- **Primary user:** Dispatcher supervisor / lead EMS worker reviewing incident response performance across five city zones.
- **Decision supported:** Which incidents and zones require investigation because response times have increased unusually relative to their historical baseline.
- **Review workflow:** Upload incident records and review zone-level monitoring results, with end-of-year summaries. Automatic live dispatch management is a future extension.
- **Prompt fit:** Incident-level monitoring exposes variation and extreme delays that monthly averages can hide. A flag requests investigation; it does not establish a cause, clinical outcome, or staff fault.

## 3. Inputs and measurement
One CSV row represents one dispatched incident. Initial datasets are synthetic and explicitly labeled as such.

| Column | Type / units | Meaning and source |
|---|---|---|
| `incident_id` | unique text | Synthetic incident identifier; an operational equivalent would come from dispatch logs. |
| `zone` | categorical, five documented labels | Incident location zone, not ambulance home zone. |
| `call_priority` | categorical | Dispatch priority; labels and ordering documented in the codebook. Required for stratified analysis. |
| `dispatch_time` | ISO 8601 timestamp with UTC offset | Time the ambulance is dispatched, from dispatch records. |
| `on_scene_time` | ISO 8601 timestamp with UTC offset | Recorded on-scene arrival, from dispatch/check-in records. |

- **Derived measurement:** `response_minutes = (on_scene_time - dispatch_time)` in minutes after converting timestamps to UTC.
- **Validation:** Report and exclude records with missing required fields, unparseable timestamps, negative durations, duplicate incident IDs, or undocumented categories. Report counts and reasons; do not silently impute values or remove valid long responses.
- **Cross-zone dispatch:** Assign analysis to the incident zone. Ambulance home zone may be an optional descriptive field; it does not change the grouping definition.
- **Analysis settings:** Explicit baseline start/end and monitoring dates; monitoring begins after the baseline ends. Confirm precise dispatch and arrival event definitions with the team before collecting operational data.
- Weather, traffic, density, speed limits, hospital distance, and GPS coordinates are not required inputs for version one.

## 4. Statistics and validation
- **Core method:** Individuals and moving-range (I-MR) charts for incident response times, separately by incident zone and call priority, in chronological order. Estimate fixed limits from an initial six-month baseline and apply them to subsequent monitoring observations.
- **Why it fits:** Each incident supplies one continuous measurement. The method monitors unusual response times and changes between consecutive incidents within a comparable group.
- **Reference:** [NIST individuals control charts](https://www.itl.nist.gov/div898/handbook/pmc/section3/pmc322.htm).
- **Individuals calculation:** Baseline mean `xbar`; consecutive absolute moving ranges with mean `MRbar`; `sigma_hat = MRbar / 1.128`; individuals limits `xbar +/- 3 * sigma_hat`. Retain mathematical limits; explain that negative response times are invalid even if the calculated lower limit is negative.
- **Primary investigation rule:** A monitoring response time strictly above its group's fixed individuals upper control limit flags that incident and its zone for investigation. Show the time, value, limit, priority, and baseline dates.
- **Variation chart:** Moving-range center `MRbar`, lower limit zero, upper limit `3.267 * MRbar`. Label upper-limit exceedances as variation signals, distinct from unusually long response times. Calculate ranges only within the same zone/priority sequence; document treatment of the baseline-to-monitoring boundary before implementation.
- **Supporting descriptive summaries:** Valid incident count, median, and 90th-percentile response time in minutes, using R's `quantile(..., type = 7)`. These require their own known-answer checks before display.
- **Zone comparison:** Present groups side by side, with sample counts, without claiming an adjusted ranking or comparing raw flag totals as equivalent evidence. Priority stratification does not adjust for weather, staffing, traffic, or other case-mix differences.
- **Assumptions:** Assess baseline stability, distribution shape, and serial dependence within each group. Use time plots, distribution diagnostics, and lag diagnostics. Strong skew or dependence can invalidate conventional limit interpretation; revisit the method before presenting validated signals if diagnostics are unsuitable. Six months is a reference window, not a full annual seasonal adjustment.
- **Minimum data:** FILL IN. Agree on a justified minimum baseline count per zone/priority, a stability review procedure, and diagnostic acceptance criteria before H-03. Until then, do not treat a merely computable chart as validated. Insufficient groups show `insufficient data` rather than a reassuring status.
- **Known-answer check:** For the arithmetic fixture `4, 5, 6, 5, 4` minutes: mean `4.8`, moving ranges `1, 1, 1, 1`, `MRbar = 1`, `sigma_hat = 1 / 1.128`, individuals UCL approximately `7.459574`, LCL approximately `2.140426`, and MR UCL `3.267`. A subsequent value `8` exceeds the individuals UCL. This five-point fixture tests arithmetic only, not baseline adequacy.
- **Degenerate checks:** Constant baseline, one observation, missing or invalid timestamps, empty groups, duplicate IDs, tied timestamps, and unequal group sizes. A zero moving range must produce an explicit degenerate-baseline status rather than divide-by-zero behavior or a validated all-clear.
- **Implementation:** Statistical calculations live in plain, framework-independent functions in `app/qc.R`; the UI calls them. Use `testthat` for known-answer and edge-case checks. Verify each reported statistic independently before styling.
- **Repository dependency:** Statistical implementation follows `.claude/skills/stats-first-steering/SKILL.md`, restored by the repository merge.

## 5. Delivery
- **Language:** R for statistical logic.
- **Proposed interface:** Shiny supervisor dashboard, starting from `demos/shinyapp/` copied once into `app/`. The team previously discussed JavaScript; confirm Shiny before scaffolding. A separate JavaScript frontend and R API are not assumed in this specification.
- **Run/deploy contract:** Provide `testme.R`, `manifestme.R`, `deployme.R`, and `app/README.md`; local startup uses one command. Supply missing scripts as part of H-04.
- **Target:** Course Posit Connect server. Keep credentials and deployment server configuration in GitHub secrets or a gitignored local environment file.
- **Deployment dependency:** Workflow templates are available under `.claude/skills/connect-publish/workflows/`; configure and verify deployment under H-06.

## 6. Outputs
- CSV validation summary with accepted/rejected counts and reasons.
- Zone and priority filters, baseline dates, monitoring period, and observation counts.
- Individuals and moving-range charts with units, fixed limits, and marked signals.
- Incident-level investigation table with the evidence triggering each flag.
- Descriptive summaries and year-end views covering only the uploaded period.
- Interpretation: `Above the upper control limit means unusually long relative to this group's baseline; investigate circumstances.`
- Distinguish `signal detected`, `no signal detected`, `insufficient data`, and `baseline unsuitable`. No signal is not proof that service is acceptable.
- No clinical or service target is invented. Control limits describe historical behavior, not acceptable service standards.

## 7. Test datasets and codebook
Create under H-02; none are claimed to exist yet:
- `data/stable.csv`: synthetic reference and monitoring periods across five zones with documented priorities and unequal call counts.
- `data/planted_delay.csv`: synthetic valid baseline followed by documented long-response incidents in one zone/priority group.
- `data/edge_cases.csv`: sparse groups, constant baseline, and deliberately invalid records to exercise explicit status and validation behavior.
- `data/README.md`: generation assumptions, every column and unit, priority/zone labels, planted events, baseline dates, expected behavior, and how equivalent operational records would be collected. Do not describe simulated calls or outcomes as real.

## 8. Documentation and reproducibility
- Root and app READMEs explain the user, scope, inputs, parameters, method, assumptions, limitations, startup command, and deployed tool link.
- `demo/README.md` walks through every dataset with independently verified expected results.
- H-03 runs `Rscript -e "testthat::test_dir('tests')"`.
- Verify startup and tests from a fresh clone, publish the team repository, and submit its link and demo on Devpost.
- H-01 remains in progress until outstanding decisions and identity fields are confirmed and the team can explain the method.

## 9. Out of scope for version one
- Live dispatcher-system APIs, automatic logging, GPS tracking, automatic arrival detection, active-call operations, and parsing private call logs.
- Logins, databases, real patient/customer data, forecasting, causal attribution, and adjusted zone league tables.
- Weather/traffic modeling, resource routing, or claims about fatalities or clinical outcomes.
- A second inferential method before the core QC calculation is independently tested.
