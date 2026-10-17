# One process-local synthetic Q-12 trip, shared across role sessions.
new_shared_responder_state <- function() shiny::reactiveValues(
  transport_phase='scene', transport_elapsed=0, chosen_hospital=NULL,
  caller_tracking_link=NULL, incident_link=NULL, hospital_arrival=NULL, demo_started=FALSE,
  demo_running=FALSE, demo_elapsed=0, demo_scenario='Rain + rush hour',
  incident_id=NULL, started_at=NULL, last_tick=NULL)

responder_trip_snapshot <- function() {
  s <- shared_responder_state
  if (!isTRUE(s$demo_started) || is.null(s$incident_id)) return(NULL)
  incident_service$revision()
  d <- incident_service$incidents[[s$incident_id]]
  if(is.null(d)) return(NULL)
  list(incident_id=d$incident_id, unit=d$ambulance_id, zone=d$zone, priority=d$call_priority,
    scenario=s$demo_scenario, scene_elapsed=d$elapsed,
    phase=if(!is.null(d$hospital_arrived_at))'hospital' else if(!is.null(d$transport_departed_at))'transport' else 'scene', transport_elapsed=if(is.null(d$transport_elapsed))0 else d$transport_elapsed,
    hospital=s$chosen_hospital, started_at=s$started_at,
    updated_at=if(is.null(s$last_tick)) s$started_at else s$last_tick,
    hospital_arrival=s$hospital_arrival)
}

# A caller-created demo selects a fresh pending assignment, preserving old records.
shared_focus_pending <- function(store, link) {
  d <- store$incidents[[link$incident_id]]
  if(is.null(d) || isTRUE(d$claimed)) stop('A new unclaimed incident is required')
  if(is.null(store$ui_state)) store$ui_state <- new_shared_responder_state()
  defaults <- shiny::isolate(shiny::reactiveValuesToList(new_shared_responder_state()))
  for(name in names(defaults)) store$ui_state[[name]] <- defaults[[name]]
  store$ui_state$incident_id <- d$incident_id
  store$ui_state$incident_link <- link
  store$ui_state$started_at <- d$dispatched_at
  invisible(d)
}
