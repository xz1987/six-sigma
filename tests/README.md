# H-03 core QC verification

Production calculations are framework-independent base R functions in
`app/qc.R`. Shiny, the simulated event feed and the experimental ETA are separate
tasks. Runtime calculations require no third-party R packages.

## Run from the repository root

Install the test-only dependency once if needed:

```r
install.packages("testthat", repos = "https://cloud.r-project.org")
```

Run the task's exact definition of done:

```sh
Rscript -e "testthat::test_dir('tests')"
```

`test-qc.R` covers the hand-calculated SPEC example, degenerate inputs,
all three H-02 CSVs, weather retention, pending calls, baseline eligibility and
review binding, fixed limits, chronological ranges, descriptive chart summaries,
and both daylight-saving transitions. Test-only review records exercise flag
gating; they are not human approval of baseline statistical suitability.

## Public API

```r
source("app/qc.R")
period <- qc_period(
  "2025-01-01T00:00:00-0500", "2026-01-01T00:00:00-0500",
  monitoring_end = "2026-02-01T00:00:00-0500"
)
raw <- read.csv("data/stable.csv", stringsAsFactors = FALSE)
result <- qc_analyze(raw, period)
group <- result$groups[["Astoria/High"]]
group$status                     # baseline requires review
group$limits                     # preliminary numerical estimates
group$diagnostics                # review inputs, not automatic approval
group$monitoring_summary          # descriptive summary remains available
result$validation$issues          # original row IDs and reasons
```

Only after a real reviewer assesses baseline diagnostics, record their name,
rationale, timestamp and approved/unsuitable decision with `qc_review()`, and pass
that record to `qc_analyze(..., reviews = list(review))`. A review binds to the
exact group's baseline records and period; changing those invalidates it.
Approvals cannot override insufficient counts, missing months or zero variation.
Disabled flags are NA, not FALSE, so unavailable analysis cannot look reassuring.

Arithmetic primitives `imr_fit()` and `imr_monitor()` intentionally allow small
known-answer fixtures. They do not check 12-month coverage or establish human
approval; applications must use `qc_analyze()` for production gating.

`qc_summary()` returns n, median and type-7 p90. `qc_box()`/`qc_boxes()` return
explicit type-7 quartiles and 1.5-IQR whiskers/markers; rendering must use these
values rather than silently substituting different plotting-library hinges.
`qc_heatmap()` expects validated completed records, returns all 168 weekday/hour
cells in New York time (Monday=1), and uses NA median with n=0 for empty cells.
Filter descriptive chart inputs by date/neighborhood/priority as needed; never
weather-filter the sequence passed to the core I-MR analysis.

Validation uses complete-record mode by default. `mode="active"` preserves
otherwise valid records with missing arrival as pending and excludes them from
completed durations. It does not implement event delivery or deduplication of
successive feed updates (H-09).

The future H-09/H-11 test files have separate commands in CONTRACT.yaml. When
those tasks land under `tests/testthat/`, explicitly wire them into the combined
suite: `testthat::test_dir('tests')` does not recursively discover nested suites.
