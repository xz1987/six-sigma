# QC and app verification

Production calculations are framework-independent base R functions in
`app/qc.R`. Shiny, the simulated event feed and the experimental ETA are separate
tasks. The core QC functions require no third-party R packages; combined app tests also
need the runtime dependencies listed in the root README.

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

## Sourced R function interface

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

The ETA/feed suite exists in `tests/testthat/`; the root suite does not recurse
into it. Run both from the repository root:

```sh
Rscript -e 'testthat::test_dir("tests", reporter="summary"); testthat::test_dir("tests/testthat", reporter="summary")'
Rscript tests/verify-caller-responder-sessions.R
Rscript tests/verify-supervisor-daily-demo.R
Rscript tests/verify-deploy-bundle.R
```

`test-supervisor-summary.R` verifies the Active tracking, History and Patterns
plain-language summaries against known counts/medians, overlapping signals,
missing goals, disabled review gates, sparse categories and empty selections.
`verify-supervisor-daily-demo.R` checks 0/1/12-case progression, summary rendering,
independent History filters, current-day exclusion, unchanged limits and separate
live-trip records. `test-supervisor-chart-guides.R` covers guides/legends for each
plot output. Existing role, route and hospital checks remain separate runnable
scripts under `tests/`; `app/caller/verify.R` covers Caller access and updates.

At commit `dc8f16f` the root suite passed 274 assertions, including 37 summary
checks. The earlier full audit's separate ETA/feed counts were 100 and 39.
The day-demo module emits 12 existing timezone-attribute warnings; its checks
pass, but this is not a warning-free browser/device or hosted-deployment sign-off.
