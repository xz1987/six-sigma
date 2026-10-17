# One process-local synthetic Q-12 trip, shared across role sessions.
new_shared_responder_state <- function() shiny::reactiveValues(
  transport_phase='scene', transport_elapsed=0, chosen_hospital=NULL,
  caller_tracking_link=NULL, hospital_arrival=NULL, demo_started=FALSE,
  demo_running=FALSE, demo_elapsed=0, demo_scenario='Rain + rush hour',
  incident_id=NULL, started_at=NULL, last_tick=NULL)

responder_trip_snapshot <- function() {
  s <- shared_responder_state
  if (!isTRUE(s$demo_started) || is.null(s$incident_id)) return(NULL)
  list(incident_id=s$incident_id, unit='Q-12', zone='Astoria', priority='High',
    scenario=s$demo_scenario, scene_elapsed=s$demo_elapsed,
    phase=s$transport_phase, transport_elapsed=s$transport_elapsed,
    hospital=s$chosen_hospital, started_at=s$started_at,
    updated_at=if(is.null(s$last_tick)) s$started_at else s$last_tick,
    hospital_arrival=s$hospital_arrival)
}

shared_trip_canonical <- function(trip) {
  if (is.null(trip) || trip$scene_elapsed < 720) return(NULL)
  stamp <- function(t) format(t, '%Y-%m-%dT%H:%M:%S+0000', tz='UTC')
  d <- response_metric_empty_records()[1, , drop=FALSE]
  d[1,] <- NA_character_
  d$incident_id <- trip$incident_id; d$zone <- trip$zone
  d$call_priority <- trip$priority; d$responder_id <- trip$unit
  d$weather_condition <- if(grepl('Snow',trip$scenario)) 'Snow/Ice' else if(grepl('rain|Rain',trip$scenario)) 'Rain' else 'Dry'
  d$call_received_time <- stamp(trip$started_at)
  d$scene_arrival_time <- stamp(trip$started_at + 720)
  if(trip$phase %in% c('transport','hospital')) d$transport_departure_time <- stamp(trip$started_at + 720)
  if(trip$phase == 'hospital') d$hospital_arrival_time <- stamp(trip$started_at + 720 + trip$transport_elapsed)
  d$response_system_source <- 'Shared synthetic Q-12 trip; simulated event timestamps'
  d
}

publish_trip_to_supervisor <- function(trip) {
  record <- shared_trip_canonical(trip)
  if(is.null(record)) return(invisible(NULL))
  old <- shiny::isolate(production_response_records())
  index <- match(record$incident_id,old$incident_id)
  if(is.na(index)) next_records <- rbind(old,record)
  else {next_records <- old; next_records[index,] <- record}
  if(!identical(old,next_records)) production_response_records(next_records)
  invisible(record)
}
