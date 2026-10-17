# H-11 experimental ETA model review

## Frozen protocol — declared before any fitting

Scope: eta-v1 synthetic fixtures only. This validates mechanics, never operational
EMS predictions. No caller/staff wiring, QC changes or fitted artifact deployment.
H-10 generator/fixtures must remain frozen. Each scenario is evaluated separately;
never pool paired incidents. Only the scenario CSV is read, never oracle truth or
complete weather history for filling missing values.

Train assignments: [2025-11-01, 2026-01-01) UTC; validation [2026-01-01,
2026-02-01); final test [2026-02-01, 2026-03-01). Derive membership from timestamps,
verify any supplied labels, unique incident IDs and event/block separation.
Training arrivals must be complete before validation begins; validation arrivals
before test begins (exclude straddling completions from those evaluation stages).
Do not refit on validation or test. Model and all gates below are frozen now;
final test outcomes cannot change formula, thresholds or training preprocessing.

Predictors: neighborhood and priority factors; peak-hour binary (7–9 and 16–18
inclusive), weekday binary (Mon–Fri), from America/New_York assignment time.
These predeclared time transforms match the illustrative generation mechanism.
Weather adds weather_condition factor (Dry reference), moderate/heavy binary
intensity contrasts (Light wet reference), sustained mph, and gust-minus-sustained
mph. Dry must have None, wet must have Light/Moderate/Heavy. Omitting a separate
Light indicator removes the Dry/None alias. Numeric wind remains continuous;
Beaufort is display-only. No identifiers, split labels, scenario, timestamps of
arrival, actual durations or truth are predictors. Arrival supplies training target
only. Formulae:

```
log_minutes ~ zone + call_priority + peak + weekday
log_minutes ~ zone + call_priority + peak + weekday + weather_condition +
              moderate + heavy + wind_speed_mph + gust_excess
```

Fit base-R ordinary least squares on identical complete-weather training records
for a fair comparison. Require >=500 eligible training rows, full column rank,
positive finite residual sigma and >=30 residual degrees of freedom. Suppress
constant/one-row/empty/rank-deficient fits. Unknown weather or missing/invalid
weather yields unavailable; no unvalidated fallback. This leaves existing QC
eligibility unchanged.

Prediction: conditional median minutes = exp(x beta). Individual nominal 95%
prediction interval = exp(x beta +/- t(.975, n-p) * s * sqrt(1+h)), with
h=x (X'X)^-1 x'. This includes future observation noise, unlike a mean confidence
interval. An optional exp(x beta + s^2/2) is labeled plug-in lognormal mean, not
median or the primary estimate. Arrival timestamps add each duration*60 to unit
assignment time; predictions depend only on assignment-time inputs.

References: [R predict.lm](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/predict.lm.html)
and [NIST individual prediction intervals](https://www.itl.nist.gov/div898/handbook/pmd/section5/pmd512.htm).
Assumptions: correct log-linear conditional mean, independent normal log errors
with constant variance and stable recording/process between periods. H-10 supplies
independent residuals conditional on weather; real event dependence invalidates
this simple interval. Episode separation does not itself prove independence.

Support: require known zone/priority, valid offset timestamps and completed target
when evaluating; valid finite nonnegative wind and gust>=sustained; observation
age [0,3600) seconds; compatible type/intensity; all categorical combinations
zone/priority/weather/intensity represented >=5 times in training. Each prediction
hour must have >=20 training observations, weekday/peak levels must be observed,
numerical sustained and gust-excess values must lie within training ranges,
and leverage <=1.5 * maximum training leverage. These are project safeguards,
not complete protection from multivariate extrapolation. Reject nonfinite outputs.

## Quantitative gates — fixed before fitting

Weather candidate must pass ALL gates on BOTH validation and untouched final test:

- Matched supported records >=500 and >=90% of each period's valid targets.
- Weather median-prediction MAE <=5 minutes and >=10% lower than otherwise
  comparable no-weather MAE, measured on the same matched records.
- Individual 95% interval empirical coverage between .90 and .98 inclusive;
  median interval width <=15 minutes.
- For each priority and each weather category (separately), >=30 matched records,
  coverage [.85,.995], median width <=20 minutes and MAE <=7 minutes.
- Training weather-model log residual absolute skew <=.5, absolute chronological
  lag-1 correlation <=.15, and max/min residual variance across priorities and
  across weather types <=4 (each level >=30). These are coarse diagnostics,
  not proof of the distribution assumptions. Inspect review numbers as well.

If validation fails, freeze that failed decision; still report test diagnostics
for the fixed model, but never promote it based on test results. If final test
fails, return experimental ETA unavailable. In the negative-control scenario we
expect no warranted >=10% benefit; report the result honestly. No simpler fallback
is authorized by this protocol. The core I-MR limits remain untouched.

Known-answer arithmetic fixture: intercept-only log outcomes [-1,0,1] gives
beta=0, s=1, df=2, h=1/3. Median=1 minute, plug-in mean=exp(.5), PI endpoints
exp(+/-4.302652729911275*sqrt(4/3)). This tests interval arithmetic only and does
not satisfy application sample-size gates. Independent tests also use a
non-intercept regression to check coefficient/variance and leverage terms.

Implementation clarifications (no gate/formula changes): skew is the third
centered residual moment divided by the second centered moment to power 3/2;
lag-1 is Pearson correlation of consecutive log residuals ordered by assignment
UTC timestamp, then incident ID. Variance ratios use sample variance (n-1).
A zero residual variance in any diagnostic group is unavailable. Constant log
response range <1e-10 or residual sigma <1e-10 is numerically degenerate.
The >=5 support rule applies to each queried compatible
zone × priority × weather × intensity combination, not impossible combinations.

## Results

Fixed protocol evaluated without tuning after either validation or test results.
Training is not refitted. No fitted model file is written; rebuild deterministically
from the frozen scenario using `eta_validate`. Both candidate models use identical
eligible complete-weather training rows. Prediction error is measured using the
conditional median and matched supported holdout records.

### weather_effect

Result: validated_synthetic
Partition rows after completion-availability filtering: train 3660, validation 1858, test 1680.
Two validation incidents complete after the test period starts and are excluded. Eligible training rows: 3518; excluded for weather inputs: 142.

| Period | Matched n | Weather MAE (min) | No-weather MAE (min) | Improvement | Weather coverage | No-weather coverage | Weather median PI width (min) | No-weather median PI width (min) |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| validation | 1769 | 1.610632 | 2.964393 | 45.667% | 0.955342 | 0.939514 | 7.640508 | 12.502799 |
| test | 1613 | 1.645948 | 3.062764 | 46.259% | 0.947303 | 0.938624 | 7.711877 | 12.407842 |

| Period | Unavailable | Gate results |
|---|---:|---|
| validation | 89 | sample PASS; availability PASS; mae PASS; weather_gain PASS; coverage PASS; width PASS; subgroups PASS |
| test | 67 | sample PASS; availability PASS; mae PASS; weather_gain PASS; coverage PASS; width PASS; subgroups PASS |

| Period | Subgroup | n | Weather MAE (min) | Coverage | Median PI width (min) |
|---|---|---:|---:|---:|---:|
| validation | call_priority: High | 593 | 1.172762 | 0.954469 | 5.481582 |
| validation | call_priority: Medium | 586 | 1.503999 | 0.962457 | 7.475828 |
| validation | call_priority: Low | 590 | 2.156637 | 0.949153 | 10.468056 |
| validation | weather_condition: Dry | 673 | 1.391624 | 0.945022 | 6.449800 |
| validation | weather_condition: Rain | 631 | 1.633993 | 0.955626 | 7.616126 |
| validation | weather_condition: Snow/Ice | 465 | 1.895902 | 0.969892 | 9.957661 |
| test | call_priority: High | 541 | 1.157226 | 0.935305 | 5.403860 |
| test | call_priority: Medium | 538 | 1.555770 | 0.957249 | 7.657875 |
| test | call_priority: Low | 534 | 2.231930 | 0.949438 | 10.359454 |
| test | weather_condition: Dry | 764 | 1.361711 | 0.943717 | 6.464691 |
| test | weather_condition: Rain | 497 | 1.869814 | 0.947686 | 8.402077 |
| test | weather_condition: Snow/Ice | 352 | 1.946788 | 0.954545 | 9.847337 |

Training log-residual diagnostics:
- skew: -0.010654409
- lag1: -0.004750402
- priority_variance_ratio: 1.039124604
- weather_variance_ratio: 1.093644475

Unavailable prediction reasons (counts for the weather candidate):
- validation: missing or invalid wind = 37; missing or unsupported intensity = 18; missing or unsupported weather = 17; wind outside training support = 17
- test: missing or invalid wind = 35; missing or unsupported intensity = 16; missing or unsupported weather = 16

| Fitted weather coefficient | Estimate |
|---|---:|
| (Intercept) | 2.084802462 |
| zoneFlushing | 0.062359870 |
| zoneJamaica | 0.096721683 |
| zoneForest Hills | -0.031969509 |
| zoneFar Rockaway | 0.144791989 |
| call_priorityMedium | 0.317202776 |
| call_priorityLow | 0.632581448 |
| peak | 0.074871946 |
| weekday | 0.028689165 |
| weather_conditionRain | 0.132835430 |
| weather_conditionSnow/Ice | 0.298493986 |
| moderate | 0.060583018 |
| heavy | 0.148471974 |
| wind_speed_mph | 0.005849447 |
| gust_excess | 0.002787300 |

### no_weather_effect

Result: unavailable
Partition rows after completion-availability filtering: train 3660, validation 1858, test 1680.
Two validation incidents complete after the test period starts and are excluded. Eligible training rows: 3518; excluded for weather inputs: 142.

| Period | Matched n | Weather MAE (min) | No-weather MAE (min) | Improvement | Weather coverage | No-weather coverage | Weather median PI width (min) | No-weather median PI width (min) |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| validation | 1769 | 1.218434 | 1.216960 | -0.121% | 0.955907 | 0.957038 | 5.857000 | 5.837046 |
| test | 1613 | 1.240009 | 1.241206 | 0.097% | 0.946683 | 0.946683 | 5.852532 | 5.837046 |

| Period | Unavailable | Gate results |
|---|---:|---|
| validation | 89 | sample PASS; availability PASS; mae PASS; weather_gain FAIL; coverage PASS; width PASS; subgroups PASS |
| test | 67 | sample PASS; availability PASS; mae PASS; weather_gain FAIL; coverage PASS; width PASS; subgroups PASS |

| Period | Subgroup | n | Weather MAE (min) | Coverage | Median PI width (min) |
|---|---|---:|---:|---:|---:|
| validation | call_priority: High | 593 | 0.882751 | 0.954469 | 4.267684 |
| validation | call_priority: Medium | 586 | 1.147221 | 0.962457 | 5.858942 |
| validation | call_priority: Low | 590 | 1.626555 | 0.950847 | 8.024163 |
| validation | weather_condition: Dry | 673 | 1.260797 | 0.946508 | 5.857000 |
| validation | weather_condition: Rain | 631 | 1.218856 | 0.955626 | 5.816767 |
| validation | weather_condition: Snow/Ice | 465 | 1.156551 | 0.969892 | 6.038835 |
| test | call_priority: High | 541 | 0.879519 | 0.935305 | 4.262155 |
| test | call_priority: Medium | 538 | 1.161333 | 0.955390 | 5.853245 |
| test | call_priority: Low | 534 | 1.684489 | 0.949438 | 8.033511 |
| test | weather_condition: Dry | 764 | 1.232916 | 0.943717 | 5.848713 |
| test | weather_condition: Rain | 497 | 1.291180 | 0.945674 | 5.842755 |
| test | weather_condition: Snow/Ice | 352 | 1.183152 | 0.954545 | 5.918261 |

Training log-residual diagnostics:
- skew: -0.010949877
- lag1: -0.004623405
- priority_variance_ratio: 1.038875037
- weather_variance_ratio: 1.093282184

Unavailable prediction reasons (counts for the weather candidate):
- validation: missing or invalid wind = 37; missing or unsupported intensity = 18; missing or unsupported weather = 17; wind outside training support = 17
- test: missing or invalid wind = 35; missing or unsupported intensity = 16; missing or unsupported weather = 16

| Fitted weather coefficient | Estimate |
|---|---:|
| (Intercept) | 2.084825185 |
| zoneFlushing | 0.062352281 |
| zoneJamaica | 0.096752409 |
| zoneForest Hills | -0.031974063 |
| zoneFar Rockaway | 0.144770675 |
| call_priorityMedium | 0.317206181 |
| call_priorityLow | 0.632564740 |
| peak | 0.074891983 |
| weekday | 0.028655995 |
| weather_conditionRain | -0.007165528 |
| weather_conditionSnow/Ice | -0.001519034 |
| moderate | 0.000625665 |
| heavy | 0.008476946 |
| wind_speed_mph | -0.000150885 |
| gust_excess | -0.000212914 |

## Reproduction and API

```r
source('app/eta.R')
d <- read.csv('data/eta/v1/weather_effect.csv')
bundle <- eta_validate(d)
assigned <- d[d$split == 'test', ]
prediction <- eta_predict(bundle, assigned)
```

`eta_validate` returns gate diagnostics, same-row comparisons, counts and only
a passing weather model. It rejects malformed timestamps, mixed/duplicate
scenarios and split/event leakage with explicit errors. `eta_fit` creates an
unpublished candidate, not a validated bundle. `eta_candidate_predict` exists
for offline evaluation and labels outputs candidate_for_evaluation. The public
`eta_predict` accepts only a validated bundle and returns row-level unavailable
reasons, median_minutes, explicitly separate mean_minutes, lower_minutes,
upper_minutes, and UTC arrival timestamps. The mean is a plug-in estimate under
the lognormal assumption; the primary median minimizes absolute-error loss.
Invalid/missing inputs remain unavailable with NA numbers. No fallback is used.

The result validates only this synthetic fixture. Formulas were informed by its
known generation structure, not discovered from unseen operational data. Shared
weather predictors do not introduce shared residual noise in H-10; applying these
intervals to dependent real incidents would require a different evaluation.
Reported subgroup coverage is descriptive, not a simultaneous coverage guarantee.
Prediction support checks reduce extrapolation but do not prove representativeness.
These finite validation/test results are not guarantees for future incidents.
For H-12, freeze each prediction on assignment and show an elapsed-window state
rather than recomputing a remaining-time forecast. Do not use these predictions
to change actual response durations, frozen QC limits or baseline approvals.

## Verification

`Rscript -e "testthat::test_file('tests/testthat/test-eta.R')"`:
`[ FAIL 0 | WARN 0 | SKIP 0 | PASS 100 ]`.
Independent review PASS (details below). No interface wiring, commit or push performed.


## Independent statistics review — PASS

Read-only reviewer independently reconstructed the weather model with normal
equations and individual prediction intervals. It did not write the implementation.

| Check | Expected | Independently observed | Verdict |
|---|---|---|---|
| Rank and training sample | Full-rank fixed formula | n=3518, p=15, residual df=3503 | PASS |
| Coefficients | Normal-equation reconstruction | Max coefficient difference 1.31e-13; sigma 0.1223384 | PASS |
| Intercept known answer | Median 1, mean exp(.5), df2 PI | 1, 1.648721, PI [0.006955132,143.7787] minutes | PASS |
| Slope known answer | Intercept 2/3, slope1, sigma sqrt(2/3), leverage11/24 | 0.6666667, 1, 0.8164966, 0.4583333 | PASS |
| Individual PI | Future-noise plus leverage variance | Max independent median/lower/upper differences 2.96e-12 / 2.33e-12 / 3.77e-12 | PASS |
| Holdout results | Match recorded metrics | Test MAE 1.645948, coverage .9473032, median width 7.711877 | PASS |
| Negative control | No spurious weather benefit | Gains -0.1211% validation / +0.0965% test; model withheld | PASS |
| Leakage | Train only; arrival target not predictor | Correct partitions, no refit; two late validation completions excluded | PASS |
| Unsupported and degenerate inputs | Unavailable, no confident numbers | All missing/unseen/sparse/rank/constant cases pass | PASS |
| Validation/test promotion | Both must pass independently | Either failing stage withholds model, even if the other passes | PASS |
| Malformed calendar/time/offset | Quiet unavailable | NA without warnings | PASS |
| Required tests | No failures or warnings | FAIL 0 / WARN 0 / SKIP 0 / PASS 100 | PASS |

The existing root QC/comparison suite also passes using
`Rscript -e 'testthat::test_dir("tests",reporter="summary")'`.
The root test_dir does not discover nested testthat files; run the H-11 command
explicitly, as required by its task definition.

Scope note: time transforms and weather contrasts are informed by the known H-10
generator. Good results here establish mechanics under an intentionally favorable
synthetic model, not real predictive validity. No operational recommendation is
supported. No passing model is supplied for the no-weather-effect scenario.
