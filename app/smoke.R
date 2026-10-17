args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep('^--file=', args, value = TRUE)
script_path <- if (length(file_arg)) sub('^--file=', '', file_arg[1]) else 'app/smoke.R'
repo_root <- normalizePath(file.path(dirname(script_path), '..'), mustWork = TRUE)

source(file.path(repo_root, 'app', 'qc.R'))
source(file.path(repo_root, 'app', 'feed.R'))
source(file.path(repo_root, 'app', 'workflow.R'))

fit <- imr_fit(c(4, 5, 6, 5, 4))
stopifnot(
  identical(fit$center, 4.8),
  isTRUE(all.equal(fit$i_ucl, 7.45957446808511, tolerance = 1e-12)),
  identical(fit$mr_ucl, 3.267)
)

records <- dashboard_load_fixture(repo_root, 'stable')
events_a <- dashboard_build_feed(records, 20261016)
events_b <- dashboard_build_feed(records, 20261016)
stopifnot(identical(events_a, events_b), nrow(events_a) > 0)
state <- feed_new(events_a)
feed_start(state)
feed_advance(state, 1)
snapshot <- feed_snapshot(state)
stopifnot(snapshot$processed_events >= 1, nrow(snapshot$pending) >= 1)

cat(sprintf('known answer: mean=%.1f I_UCL=%.6f MR_UCL=%.3f\n',
  fit$center, fit$i_ucl, fit$mr_ucl))
cat(sprintf('scenario: stable.csv; monitoring incidents=%d; replay events=%d; seed=%d\n',
  nrow(dashboard_source_monitoring(records)), nrow(events_a), 20261016))
cat(sprintf('feed loaded: processed=%d active=%d completed=%d\n',
  snapshot$processed_events, nrow(snapshot$pending), nrow(snapshot$completed)))
cat('SMOKE PASS\n')
