# H-10 independent statistics review — current hourly/episode fixture

Fresh-context read-only stats-reviewer independently reviewed the current
uncommitted v1. Overall: PASS; ready for team check-in. Earlier fixture drafts
are superseded by the hourly-block / variable-length episode design below.

| Check | Expected | Independently observed | Verdict |
|---|---|---|---|
| Hourly linkage | Latest observation, matching episode | 2880 unique hour blocks; 7200 exact floor-hour lookups | PASS |
| Episode structure | Contiguous, fixed type, 3–12 h except boundary clipping | 392 episodes, observed lengths 1–12 h; short episodes at split boundaries only | PASS |
| Separation | Whole observation history has no split-crossing episode | 201 / 109 / 82 episodes per split, zero leakage | PASS |
| Daily groups | 60 calls/day; 15 groups four times/day | All 120 UTC days pass | PASS |
| Hourly calls | Two or three/hour | 1440 hours with two, 1440 with three | PASS |
| Pairing | Identical covariates and noise | All paired predictors/splits identical | PASS |
| Equations | Reconstruct both scenarios | All 14400 durations exact, zero mismatches | PASS |
| Hand examples | 480 / 632 s | Both reproduced | PASS |
| Full-day coverage | All 24 local hours in each split | Hour counts train 141–160, validation 73–82, test 65–73 | PASS |
| Duration | Positive | Effect 338–2698 s; no-effect 324–1590 s | PASS |
| Reproducibility | Four files byte-identical | Seed 5310 confirmed | PASS |
| Missing weather | 281 issues, no QC exclusion | 74+71+69+67, 7200 retained | PASS |
| Beaufort and degenerate weather | Documented boundaries; invalid unavailable | Validator passes below/at/above, empty, missing, nonfinite and calm | PASS |
| Required check | Exit 0 | Eight PASS lines | PASS |

Actual `Rscript data/eta/validate.R` output:

```text
PASS: 2880 hourly weather updates; latest prior observation per incident; no future weather; hourly blocks and variable-length weather episodes verified
PASS: versioned schema; 7200 paired incidents per scenario; positive durations; assignment-time weather
PASS: seed 5310 reproduces all four CSV files byte for byte
PASS: chronological train/validation/test = 3660/1860/1680; disjoint variable-length weather episodes; all 24 local hours per split (at least 20 incidents/hour)
PASS: all 12 Beaufort boundaries below/at/above; negative/missing/nonfinite/empty wind stays unavailable
PASS: 281 weather field issues reported; all 7200 incidents remain QC eligible; zero/missing/invalid/empty checks
PASS: known injected log equations recovered exactly after second rounding; calm example 480 seconds; rain example 632 seconds
PASS: H-10 synthetic fixtures validated; no ETA model or operational accuracy claim
```

Independent reconstruction maximum base/weather errors: 4.44e-15 / 1.11e-16.
Observation ages: train 0–3599 s, validation 2–3599 s, test 0–3594 s.
Primary-agent Git hashes confirmed all three original H-02 CSVs unchanged.

H-11 limitations: do not pool paired scenarios or use truth/IDs as predictors;
resolve Dry/None redundancy; check support for joint predictor combinations.
Episode durations/dynamics and split-boundary resets are invented conventions,
not meteorological validation. No operational EMS accuracy is established.
