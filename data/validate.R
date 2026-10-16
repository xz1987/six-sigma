# Fixture audit, not the production QC implementation. Run from repo root.
columns <- c('incident_id','zone','call_priority','weather_condition','dispatch_time','on_scene_time')
zones <- c('Astoria','Flushing','Jamaica','Forest Hills','Far Rockaway')
priorities <- c('High','Medium','Low')
parse_time <- function(x) as.POSIXct(x, format = '%Y-%m-%dT%H:%M:%S%z', tz = 'UTC')
audit <- function(file) {
  d <- read.csv(file, colClasses = 'character', na.strings = '', check.names = FALSE)
  stopifnot(identical(names(d), columns))
  start <- parse_time(d$dispatch_time); end <- parse_time(d$on_scene_time)
  dup <- duplicated(d$incident_id) | duplicated(d$incident_id, fromLast = TRUE)
  valid <- !is.na(d$incident_id) & d$zone %in% zones & d$call_priority %in% priorities &
    !is.na(start) & !is.na(end) & !dup
  valid[is.na(valid)] <- FALSE
  valid <- valid & !is.na(end - start) & as.numeric(difftime(end, start, units = 'secs')) >= 0
  valid[is.na(valid)] <- FALSE
  weather_issue <- is.na(d$weather_condition) | !d$weather_condition %in% c('Dry','Rain','Snow/Ice','Unknown')
  d$weather_condition[weather_issue] <- 'Unknown'
  d <- d[valid, ]; start <- start[valid]; end <- end[valid]
  d$minutes <- as.numeric(difftime(end, start, units = 'mins'))
  local <- format(start, '%Y-%m-%d', tz = 'America/New_York')
  d$baseline <- local >= '2025-01-01' & local < '2026-01-01'
  d$month <- format(start, '%Y-%m', tz = 'America/New_York')
  d <- d[order(start, d$incident_id), ]
  cat(sprintf('%s: rows=%d valid=%d rejected=%d weather_to_Unknown=%d\n', basename(file), length(valid), sum(valid), sum(!valid), sum(weather_issue & valid)))
  list(data = d, valid = valid, weather_issue = weather_issue)
}
# Independent hand-calculated arithmetic check from SPEC section 4.
known <- c(4,5,6,5,4)
stopifnot(mean(known) == 4.8, mean(abs(diff(known))) == 1,
 abs((mean(known) + 3 * mean(abs(diff(known))) / 1.128) - 7.459574468085106) < 1e-12)
s <- audit('data/stable.csv'); p <- audit('data/planted_delay.csv'); e <- audit('data/edge_cases.csv')
stopifnot(length(s$valid) == 3465, all(s$valid), all(p$valid), sum(e$valid) == 261, sum(!e$valid) == 9,
          sum(e$weather_issue & e$valid) == 2)
# Prove baseline counts and every-month coverage for all 15 groups.
for (z in zones) for (priority in priorities) {
  b <- subset(s$data, baseline & zone == z & call_priority == priority)
  stopifnot(nrow(b) >= 100, length(unique(b$month)) == 12)
}
stopifnot(sum(s$data$baseline) == 3060, sum(!s$data$baseline) == 405)
stopifnot(identical(subset(s$data, baseline), subset(p$data, baseline)))
a <- subset(s$data, zone == 'Astoria' & call_priority == 'High')
b <- a$minutes[a$baseline]
center <- mean(b); mr <- mean(abs(diff(b))); ucl <- center + 3 * mr / 1.128
sa <- subset(a, !baseline)
pa <- subset(p$data, !baseline & zone == 'Astoria' & call_priority == 'High')
changed <- pa$incident_id[pa$minutes != sa$minutes]
stopifnot(setequal(changed, c('M-G01-0011','M-G01-0007','M-G01-0013')),
 all(pa$minutes[pa$incident_id %in% changed] == 30),
 all(pa$minutes[pa$incident_id %in% changed] > ucl))
cat(sprintf('Astoria/High baseline: n=%d mean=%.9f MRbar=%.9f individuals_UCL=%.9f MR_UCL=%.9f\n', length(b), center, mr, ucl, 3.267 * mr))
# These are observed fixture signals, not proof of model suitability or approval.
count_signals <- function(d) {
  i <- m <- 0L
  for (z in zones) for (priority in priorities) {
    g <- subset(d, zone == z & call_priority == priority)
    b <- g$minutes[g$baseline]; v <- g$minutes[!g$baseline]
    r <- mean(abs(diff(b)))
    i <- i + sum(v > mean(b) + 3 * r / 1.128)
    m <- m + sum(abs(diff(c(tail(b,1),v))) > 3.267 * r)
  }
  c(individuals = i, moving_range = m)
}
ss <- count_signals(s$data); ps <- count_signals(p$data)
cat('Stable monitoring upper-limit exceedances:', paste(names(ss), ss, collapse = ', '), '\n')
cat('Planted monitoring upper-limit exceedances:', paste(names(ps), ps, collapse = ', '), '\n')
stopifnot(all(ss == c(1,5)), all(ps == c(4,6)))
# Degenerate and sparse baselines must never be treated as approved.
stopifnot(nrow(subset(e$data, baseline & zone == 'Astoria' & call_priority == 'High')) == 120,
 all(subset(e$data, baseline & zone == 'Flushing' & call_priority == 'Medium')$minutes == 10),
 length(unique(subset(e$data, baseline & zone == 'Flushing' & call_priority == 'Medium')$month)) == 12,
 nrow(subset(e$data, baseline & zone == 'Jamaica' & call_priority == 'Low')) == 12,
 nrow(subset(e$data, baseline & zone == 'Far Rockaway' & call_priority == 'High')) == 1)
stopifnot(all(c('E-M-02','E-M-03') %in% e$data$incident_id))
# Known small summary fixture: 8,9,10,8,9 -> median=9, p90(type7)=9.6.
v <- subset(e$data, !baseline & zone == 'Astoria' & call_priority == 'High')$minutes
stopifnot(median(v) == 9, abs(unname(quantile(v,.9,type=7)) - 9.6) < 1e-10)
cat('PASS: 15 groups, 12 baseline months, identical baselines, planted delays, edge cases, weather retention.\n')
