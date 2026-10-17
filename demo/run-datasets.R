# H-05: reproduce the existing QC tool on committed synthetic fixtures.
# Run from repository root: Rscript demo/run-datasets.R [stable|planted|edge]
source('app/qc.R')
period <- qc_period('2025-01-01T00:00:00-0500', '2026-01-01T00:00:00-0500',
                    '2026-01-01T00:00:00-0500', '2026-02-01T00:00:00-0500')
files <- c(stable='stable.csv', planted='planted_delay.csv', edge='edge_cases.csv')
args <- commandArgs(trailingOnly=TRUE)
if (length(args)>1 || (length(args)==1 && !args %in% names(files))) stop('Choose stable, planted or edge, or omit for all three.')
selected <- if(length(args)) args else names(files)
results <- list()
near <- function(x,y) stopifnot(isTRUE(all.equal(x,y,tolerance=1e-8)))
cat('SYNTHETIC DATA ONLY; historical assignment-to-scene measurements in minutes.\n')
cat('No operational baseline approval is created. Exceedances below are arithmetic checks.\n')
for (scenario in selected) {
  raw <- read.csv(file.path('data',files[[scenario]]),stringsAsFactors=FALSE)
  a <- qc_analyze(raw,period)
  results[[scenario]] <- a
  v <- a$validation
  cat(sprintf('\n%s: input=%d accepted=%d rejected=%d weather_normalized=%d\n',
              files[[scenario]],nrow(raw),v$counts['accepted'],v$counts['rejected'],v$counts['weather_normalized']))
  cat(sprintf('baseline=%d monitoring=%d\n',sum(vapply(a$groups,function(g)nrow(g$baseline),integer(1))),
              sum(vapply(a$groups,function(g)nrow(g$monitoring),integer(1)))))
  print(table(vapply(a$groups,function(g)g$status,character(1))))
  stopifnot(all(vapply(a$groups,function(g)!g$flags_enabled,logical(1))))
  g <- a$groups[['Astoria/High']]
  cat(sprintf('Astoria/High: center=%.9f I_UCL=%.9f MR_UCL=%.9f\n',g$limits$center,g$limits$i_ucl,g$limits$mr_ucl))
  near(g$limits$center,8.159166666666667); near(g$limits$i_ucl,11.456815512545445)
  near(g$limits$mr_ucl,4.050805462184874)
  if (scenario!='edge') {
    # Numerical demonstration only; leave qc_analyze review gate unchanged.
    checks <- lapply(a$groups,function(g)imr_monitor(g$limits,g$monitoring$response_minutes))
    i <- sum(vapply(checks,function(x)sum(x$individual_signal),integer(1)))
    mr <- sum(vapply(checks,function(x)sum(x$variation_signal),integer(1)))
    cat(sprintf('Arithmetic exceedances (not approved alerts): Individuals=%d Moving_range=%d\n',i,mr))
    ids <- unlist(Map(function(g,x)g$monitoring$incident_id[x$individual_signal],a$groups,checks),use.names=FALSE)
    cat('Individuals IDs:',paste(ids,collapse=', '),'\n')
    stopifnot(v$counts['accepted']==3465,v$counts['rejected']==0,
              i==if(scenario=='stable')1 else 4,mr==if(scenario=='stable')5 else 6)
  } else {
    s <- g$monitoring_summary
    cat(sprintf('Astoria/High monitoring: n=%d median=%.1f p90=%.1f\n',s$n,s$median,s$p90))
    cat('Astoria/High assignment order:',paste(g$monitoring$incident_id,collapse=', '),'\n')
    cat('Unknown weather monitoring records:',sum(v$completed$weather_condition=='Unknown' & grepl('^E-M-',v$completed$incident_id)),'\n')
    print(v$issues[,c('incident_id','reason','excluded')],row.names=FALSE)
    stopifnot(v$counts['accepted']==261,v$counts['rejected']==9,v$counts['weather_normalized']==2,
              a$groups[['Flushing/Medium']]$status=='degenerate baseline',s$n==5)
    near(s$median,9);near(s$p90,9.6)
  }
}
if(all(c('stable','planted') %in% names(results))) {
  stopifnot(identical(lapply(results$stable$groups,function(g)g$limits),
                      lapply(results$planted$groups,function(g)g$limits)))
  cat('\nPASS: stable and planted baseline limits are identical.\n')
}
cat('H-05 DATASET DEMO PASS\n')
