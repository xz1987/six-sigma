# ETA fixture v1 — entirely synthetic

H-10 supplies controlled test data for a later H-11 model. These are invented
incidents and weather, not Queens operational records, observations, or estimates
of real weather effects. No ETA model is fitted here. H-02 CSVs are untouched.

Run from the repository root with base R:

```sh
Rscript data/eta/generate.R
Rscript data/eta/validate.R
```

Validation also sources the existing `app/qc.R` to check retention of incidents.
Generator seed is 5310; versioned outputs are in `v1/`:

| File | Rows | Purpose |
|---|---:|---|
| weather_effect.csv | 7,200 | Invented positive weather relationship |
| no_weather_effect.csv | 7,200 | Paired counterfactual with weather coefficients all zero |
| truth.csv | 7,200 | Generation audit only; forbidden as predictor input |
| weather_observations.csv | 2,880 | Hourly blocks grouped into variable-length weather episodes; full synthetic audit |

The paired scenarios share IDs, predictors and random noise. They are alternative
worlds for the same invented incidents: never pool them as independent records.
The second scenario has no structural weather effect; finite-sample associations
can still arise by chance. It is not guaranteed that a fitted model will recover
zero coefficients or pass an improvement gate. H-11 must predeclare its gates.

## Codebook: both scenario CSVs

| Column | Type / units | Definition |
|---|---|---|
| schema_version | text | Always eta-v1 |
| synthetic | logical | Always TRUE |
| incident_id | text | Unique ETA-V1 identifier within a scenario |
| weather_event_id | text | WX-V1 synthetic weather episode spanning related hourly blocks; never a predictor |
| weather_block_id | text | HOUR-V1 unique UTC hour shared by two or three incidents; never a predictor |
| zone | category | Astoria, Flushing, Jamaica, Forest Hills, Far Rockaway; invented incident neighborhood |
| call_priority | category | High, Medium, Low; synthetic urgency, not official EMS triage |
| weather_condition | category | Dry, Rain, Snow/Ice; raw blank intentionally allowed; cleaner maps invalid/missing to Unknown |
| wind_speed_mph | numeric or raw text defect | Synthetic sustained speed, 0–30 mph before defects; represents 2-minute mean ending at weather_observed_at |
| wind_gust_mph | numeric or raw text defect | Synthetic maximum 3-second gust in preceding 10 minutes ending at weather_observed_at; sustained plus 0–15 mph before defects |
| precipitation_intensity | category | None for Dry; Light, Moderate, Heavy otherwise, following hourly persistence rules; invalid unrated defects become Unknown |
| weather_observed_at | ISO 8601 offset timestamp | Latest hourly observation at or before assignment; all weather inputs are available by assignment |
| dispatch_time | ISO 8601 offset timestamp | UTC unit assignment, 0–3599 seconds after hourly block start |
| split | category | train, validation, test; predetermined chronological partition |
| scenario | category | weather_effect or no_weather_effect |
| on_scene_time | ISO 8601 offset timestamp | Assignment plus generated positive duration, rounded to nearest second; target only |

These observation windows are fixture conventions, not claims that source weather
is measured by an operational EMS system. No wind or weather API is used. Intensity
labels have no mm/hour or snowfall thresholds and are NOT official ratings. There
is no combined official wind/precipitation severity score or winter-storm index.
There are no coordinates, real addresses, patients or hospital destinations.

## Generation equations

Times below are minutes; equations use natural logarithms. For each incident:

```
base = log(priority_minutes) + neighborhood_offset
       + 0.08 * peak_hour + 0.03 * weekday
weather_effect = 0.12 * Rain + 0.28 * SnowIce
       + intensity_offset + 0.006 * sustained_mph
       + 0.003 * (gust_mph - sustained_mph)
epsilon ~ Normal(0, 0.12^2), independently per incident
weather scenario seconds = round(60 * exp(base + weather_effect + epsilon))
no-weather scenario seconds = round(60 * exp(base + epsilon))
```

Priority minutes: High 8, Medium 11, Low 15. Neighborhood log offsets: Astoria 0,
Flushing 0.06, Jamaica 0.10, Forest Hills -0.04, Far Rockaway 0.15. Peak hours are
7–9 and 16–18 inclusive; weekday is Monday–Friday. Both use America/New_York
assignment time, including DST. Intensity log offsets: None 0, Light 0.02,
Moderate 0.08, Heavy 0.16. Each synthetic episode draws a duration uniformly from integers 3–12 hours;
it is clipped at a partition boundary so episodes cannot cross train/validation/test.
Episodes can therefore be shorter than three hours at a boundary. Each draws
one weather type with probabilities Dry .5, Rain .3, Snow/Ice .2, held through
that episode. Adjacent episodes can have the same type but independent starts.
Dry intensity is None. Wet intensity starts uniformly over Light, Moderate, Heavy,
then is retained hourly with probability .7 or redrawn uniformly otherwise.
Initial sustained wind is Uniform(0,30) mph; subsequent hourly wind adds
Uniform(-4,4), capped to [0,30]. Gust is sustained plus an independent
Uniform(0,15) excess at each observation. Wind values round to 0.001 mph.
These invented episodes provide a meaningful synthetic grouping mechanism;
none of the dynamics or durations are meteorologically validated.
All 15 neighborhood/priority combinations occur four times per UTC day.

Before second rounding, exp(base + weather_effect) is the conditional median;
the lognormal conditional mean additionally multiplies by exp(0.12^2/2).
The same epsilon is used in each paired scenario. Weather predictors are shared within each hourly observation and related across
hours within events, but residuals have no extra event-level random effect. This is a
simplified simulation, not a claim of independent real emergency incidents.

Hand example: Astoria/High, weekend off-peak, calm Dry/None, epsilon=0 gives
8 minutes = 480 seconds. Rain/Moderate, sustained 10 and gust 15 gives log effect
0.12 + 0.08 + 0.06 + 0.015 = 0.275; round(480 * exp(0.275)) = 632 seconds.

`truth.csv` columns: incident_id (join key), base_log_minutes (base above),
weather_log_effect (effect above), noise_log_minutes (epsilon), true_wind_speed_mph,
true_wind_gust_mph (pre-defect mph), true_weather_condition and
true_precipitation_intensity (pre-defect labels). Truth columns, scenario, IDs,
arrival timestamps, eventual durations and split labels must never enter an ETA
predictor matrix. Ground truth exists for checks only, not missing-data imputation.

## Frozen chronological / event splits

All timestamps below are UTC, with end exclusive:

| Split | Assignment period | Incidents per scenario | Hourly blocks |
|---|---|---:|---:|
| train | 2025-11-01 to 2026-01-01 | 3,660 | 1,464 |
| validation | 2026-01-01 to 2026-02-01 | 1,860 | 744 |
| test | 2026-02-01 to 2026-03-01 | 1,680 | 672 |

There are 2,880 hourly blocks over 120 days. Each UTC day has two calls per hour
plus one extra in 12 randomly selected distinct hours, totaling 60 calls/day.
The 60 neighborhood/priority labels (four copies of all 15 combinations) are
shuffled across those hours; assignment seconds are sampled uniformly from 0–3599.
Every split contains all 24 America/New_York hours, with at least 20 calls/hour.
All hourly blocks belonging to one simulated episode stay in one partition.
The complete observation history, including every hour, is checked for separation.
Both paired scenarios use identical partition
membership. Every split contains all 15 groups and all nonmissing weather and
intensity categories. Do not randomly split individual incidents. For H-11, fit
on train, tune on validation and freeze choices before opening test outcomes.
H-10's structural equation audit necessarily reads test outcomes; it is not model
selection or a test-set performance evaluation. Do not tune the generator after
seeing model results. Changing the fixture requires a new version and review.

## Weather defects and cleaning

After generating durations from complete latent weather, reporting defects are
injected by original incident index: every 97th sustained value is blank (74),
every 101st gust is -1 (71), every 103rd intensity is unrated (69), and every
107th weather label is blank (67). These total 281 field issues; a row may have
multiple issues. Records are sorted by assignment after injection.

`eta_clean_weather(df)` returns `list(data, issues)`. Issues list row (position in
the supplied frame), field and reason. Missing/invalid/nonfinite/negative numeric
wind becomes NA; a gust below known sustained speed becomes NA with
below_sustained issue. Legitimate zero remains zero. Invalid/missing weather or
intensity becomes Unknown. Missing columns are reported for every row. No rows
are removed. Unknown is distinct from Dry/None. No precipitation consistency
repair or guessed intensity is applied. ETA eligibility/fallback is H-11's job;
all 7,200 otherwise valid incidents remain eligible for existing QC. Do not use
this weather-dependent fixture to approve fixed I-MR baselines.

## Beaufort display conversion

Reference: [NWS Boise Beaufort Wind Scale](https://www.weather.gov/boi/beaufort),
accessed 2026-10-16. Its published whole-mph ranges yield force 0 for <1, then
1–4, 5–7, 8–12, 13–18, 19–24, 25–31, 32–38, 39–46, 47–54, 55–63,
64–72, and >=73 for forces 1–12 respectively.

For fractional mph we explicitly adopt left-closed bins at lower bounds
0, 1, 5, 8, 13, 19, 25, 32, 39, 47, 55, 64, 73. For example 4.999 mph maps to
force 1 and 5 mph to force 2. This is a documented project extension of the
whole-mph table, not a claim that NWS specifies these fractional boundaries.
`eta_beaufort(sustained_mph)` returns integer force or NA for invalid/missing wind;
`eta_clean_weather` adds derived `beaufort_force`. Never apply to gusts. Never
use the force as a linear minutes-of-delay feature. Tests cover below/at/above
all 12 boundaries and invalid, empty, calm and extreme values.

## Expected verification and limitations

`Rscript data/eta/validate.R` prints eight PASS lines: schema/positive durations,
hourly updates/latest-prior observation, byte-identical regeneration, chronological/event separation, Beaufort boundaries,
281 field issues with QC retention, independent equation reconstruction/hand
examples, and the overall H-10 check. No dependencies beyond base R and existing
QC source. No model coefficients, MAE or interval coverage are claimed here.

Weather and intensity encoding is not fully unconstrained: Dry always has None,
while wet labels never have None. H-11 must address resulting design-matrix
redundancy explicitly rather than silently accepting aliased coefficients. All
invented effects and durations omit geography within neighborhoods, travel
distance, staffing, traffic and real seasonal dynamics. Successful recovery of
these equations tests software mechanics only, never operational EMS accuracy.

Independent review is recorded in [REVIEW.md](REVIEW.md). Full-day hour coverage
is tested explicitly. Weather now updates in hourly blocks within variable-length weather episodes.
Each incident uses the latest observation at or before assignment, less than one
hour old; duration generation uses that observation only, never future weather. H-11 must check combinations and conditions for support:
hour coverage alone does not establish operational representativeness.

### Hourly weather audit

`v1/weather_observations.csv` contains `weather_event_id` (episode grouping key),
`weather_block_id` (unique hour), `weather_observed_at` (UTC hour start),
`weather_condition`, `precipitation_intensity`, `wind_speed_mph`, and
`wind_gust_mph` (same units/windows as above, no reporting defects). This is
complete synthetic source history, not an input for repairing missing incident
weather. Multiple calls in an hour share the same observation. At an exact hour
boundary, the new observation is selected; future observations are never used.

This is still uncommitted v1 under development; no H-11 model has been fitted to
its earlier iterations. Hourly blocks describe observation cadence, while episode
IDs describe related weather across hours. The chronological split already gives
temporal separation; episode checks additionally verify no designed shared episode
crosses a split. Splits intentionally reset episode generation, a synthetic
simplification rather than a guarantee that real storms end at month boundaries.
