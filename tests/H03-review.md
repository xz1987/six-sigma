# H-03 independent statistical review — 2026-10-16

Read-only stats-reviewer independently executed the actual functions, reran the
required test command, and cross-checked estimates and lag correlation in Python.
Scope is the QC core, not the future ETA model, event feed or Shiny rendering.

| Statistic/check | Expected | Observed | Verdict |
|---|---|---|---|
| Mean of 4,5,6,5,4 | 4.8 | 4.8 | PASS |
| Consecutive moving ranges / MR mean | 1,1,1,1 / 1 | Same | PASS |
| Sigma = MRbar/1.128 | 0.886524823 | 0.886524823 | PASS |
| Individuals LCL / UCL | 2.140425532 / 7.459574468 | Same | PASS |
| MR LCL / UCL | 0 / 3.267 | Same | PASS |
| Monitoring8, boundary MR | I flag TRUE, MR4, variation TRUE | Same | PASS |
| Equality at upper limit | No signal | FALSE | PASS |
| Summary of 8,9,10,8,9 | n5, median9, type7 p90=9.6 | Same | PASS |
| Type7 quartiles of 1:4 | 1.75 / 2.5 / 3.25 | Same | PASS |
| Box 1,2,3,4,100 | Whiskers1/4, marker100 | Same | PASS |
| Sparse weather boxes | Dry1 Rain1 Snow/Ice0 Unknown3 | Same; empty box values NA | PASS |
| Edge heatmap | Monday10h n5 median9;167 empty cells | Same; empty medians NA | PASS |
| DST elapsed interval | 2 minutes across spring jump | 2 | PASS |
| DST aggregation | Sunday01h n3 median6;03h n1 median4;02h empty | Same | PASS |
| Astoria/High baseline lag1 correlation | Python -0.07358105667597928 | R -0.0735810566759793 | PASS |
| Eligibility | All15 groups cover12 months; n110 with missing month fails | Same | PASS |
| Unreviewed or changed baseline | Flags disabled; stale review rejected | baseline requires review | PASS |
| Frozen stable/planted baseline | Mean8.159166667 MRbar1.239915966 UCL11.456815513 MRUCL4.050805462 | Same | PASS |
| Stable CSV | Accepted3465 rejected0; I/MR signals1/5 | Same | PASS |
| Planted CSV | Accepted3465 rejected0; I/MR signals4/6 | Same | PASS |
| Edge CSV | Accepted261 rejected9; weather normalization2 | Same | PASS |
| Duplicate IDs | Reject both ambiguous rows | Two duplicate issues | PASS |
| Unknown weather | Three monitoring records retained as Unknown | Same | PASS |
| Chronology | Ties by ID; E-M-04 MR1 becomes2 when predecessor completes | Same; boundary MR3.016667 | PASS |
| Constant / one / empty baseline | Explicit status; undefined usable limits; disabled flags | NA sigma/UCL/signals | PASS |
| Active mode | Missing arrival pending | Accepted262 completed261 pending1 rejected8 | PASS |
| Required tests | No failures/warnings/skips | FAIL0 WARN0 SKIP0 PASS88 | PASS |

**OVERALL: PASS.** No blocking statistical concerns found in this review.

Test command from the repository root:

```sh
Rscript -e "testthat::test_dir('tests')"
```

Actual output:

```text
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 88 ]
```

This verifies formulas, validation logic and approval mechanics. It does not
constitute genuine human baseline suitability approval, operational validation,
or proof that weather/seasonality assumptions hold for real EMS data. Test-only
approval records are not supplied as production approvals. Raw low-level
arithmetic primitives are for tests/calculation; the application must call the
grouped, gated qc_analyze API.
