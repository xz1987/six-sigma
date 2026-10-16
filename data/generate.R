# Synthetic fixtures only. Run from repository root: Rscript data/generate.R
# Base R; does not implement the application's QC functions.
set.seed(5300)
zones <- c('Astoria', 'Flushing', 'Jamaica', 'Forest Hills', 'Far Rockaway')
priorities <- c('High', 'Medium', 'Low')
groups <- expand.grid(zone = zones, call_priority = priorities, stringsAsFactors = FALSE)
weather <- c('Dry', 'Rain', 'Snow/Ice', 'Unknown')
base_means <- c(High = 480, Medium = 660, Low = 900) # invented seconds
stamp <- function(x) format(x, '%Y-%m-%dT%H:%M:%S%z', tz = 'America/New_York')
make_rows <- function(g, n, year, prefix, baseline = FALSE) {
  # Month coverage is guaranteed by round-robin allocation, not random luck.
  month <- if (baseline) rep(1:12, length.out = n) else rep(1L, n)
  day <- sample(1:28, n, replace = TRUE)
  hour <- sample(0:23, n, replace = TRUE)
  minute <- sample(0:59, n, replace = TRUE)
  # Noon-safe dates avoid DST transition dates (March 9 and November 2, 2025).
  day[month == 3 & day == 9] <- 10
  day[month == 11 & day == 2] <- 3
  time <- as.POSIXct(sprintf('%d-%02d-%02d %02d:%02d:00', year, month, day, hour, minute), tz = 'America/New_York')
  seconds <- round(rnorm(n, base_means[groups$call_priority[g]], 60))
  stopifnot(all(seconds > 0)) # fail rather than silently truncate or resample
  out <- data.frame(incident_id = sprintf('%s-G%02d-%04d', prefix, g, seq_len(n)),
    zone = groups$zone[g], call_priority = groups$call_priority[g],
    weather_condition = sample(weather, n, replace = TRUE, prob = c(.65, .25, .08, .02)),
    dispatch_time = stamp(time), on_scene_time = stamp(time + seconds), stringsAsFactors = FALSE)
  # Winter labels are limited to Nov-Mar; weather does not change durations.
  out$weather_condition[!month %in% c(11,12,1,2,3) & out$weather_condition == 'Snow/Ice'] <- 'Rain'
  out[order(time, out$incident_id), ]
}
sort_rows <- function(x) x[order(as.POSIXct(x$dispatch_time, format = '%Y-%m-%dT%H:%M:%S%z', tz = 'UTC'), x$incident_id), ]
baseline <- do.call(rbind, lapply(1:15, function(g) make_rows(g, 120 + 12 * (g - 1), 2025, 'B', TRUE)))
monitor <- do.call(rbind, lapply(1:15, function(g) make_rows(g, 20 + g - 1, 2026, 'M')))
stable <- sort_rows(rbind(baseline, monitor))
planted <- stable
# Replace the last three monitored Astoria/High durations with exactly 30 minutes.
target <- which(planted$zone == 'Astoria' & planted$call_priority == 'High' & startsWith(planted$incident_id, 'M-'))
target <- tail(target, 3)
planted$on_scene_time[target] <- stamp(as.POSIXct(planted$dispatch_time[target], format = '%Y-%m-%dT%H:%M:%S%z', tz = 'UTC') + 1800)

edge_base <- baseline[baseline$zone == 'Astoria' & baseline$call_priority == 'High', ]
constant <- baseline[baseline$zone == 'Flushing' & baseline$call_priority == 'Medium', ]
constant <- constant[seq(1, nrow(constant), length.out = 120), ]
constant$on_scene_time <- stamp(as.POSIXct(constant$dispatch_time, format = '%Y-%m-%dT%H:%M:%S%z', tz = 'UTC') + 600)
sparse <- baseline[baseline$zone == 'Jamaica' & baseline$call_priority == 'Low', ]
sparse <- sparse[!duplicated(substr(sparse$dispatch_time, 1, 7)), ]
singleton <- baseline[baseline$zone == 'Far Rockaway' & baseline$call_priority == 'High', ][1, ]
edge_base <- rbind(edge_base, constant, sparse, singleton)
edge_base$incident_id <- paste0('E-', edge_base$incident_id)
edge_monitor <- data.frame(incident_id = sprintf('E-M-%02d', 1:8),
 zone = c(rep('Astoria', 5), 'Flushing', 'Jamaica', 'Far Rockaway'),
 call_priority = c(rep('High', 5), 'Medium', 'Low', 'High'),
 weather_condition = c('Dry', '', 'Hail', 'Unknown', 'Rain', 'Snow/Ice', 'Dry', 'Dry'),
 dispatch_time = sprintf('2026-01-05T10:%02d:00-0500', c(0,0,1,2,3,4,5,6)),
 on_scene_time = sprintf('2026-01-05T10:%02d:00-0500', c(8,9,11,10,12,14,16,17)), stringsAsFactors = FALSE)
bad <- edge_monitor[rep(1, 9), ]
bad$incident_id <- c('E-BAD-DUP', 'E-BAD-DUP', 'E-BAD-NEGATIVE', 'E-BAD-DISPATCH', 'E-BAD-ARRIVAL', 'E-BAD-TIMESTAMP', 'E-BAD-ZONE', 'E-BAD-PRIORITY', '')
bad$on_scene_time[3] <- '2026-01-05T09:59:00-0500'
bad$dispatch_time[4] <- ''
bad$on_scene_time[5] <- ''
bad$dispatch_time[6] <- 'not-a-timestamp'
bad$zone[7] <- 'Brooklyn'
bad$call_priority[8] <- 'Critical'
edge <- rbind(sort_rows(rbind(edge_base, edge_monitor)), bad)
write.csv(stable, 'data/stable.csv', row.names = FALSE, na = '')
write.csv(planted, 'data/planted_delay.csv', row.names = FALSE, na = '')
write.csv(edge, 'data/edge_cases.csv', row.names = FALSE, na = '')
cat(sprintf('Generated stable=%d, planted_delay=%d, edge_cases=%d rows.\n', nrow(stable), nrow(planted), nrow(edge)))
cat('Planted 30-minute incidents:', paste(planted$incident_id[target], collapse = ', '), '\n')
