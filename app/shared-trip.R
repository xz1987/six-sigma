# One process-local synthetic Q-12 trip, shared across role sessions.
new_shared_responder_state <- function() shiny::reactiveValues(
  transport_phase='scene', transport_elapsed=0, chosen_hospital=NULL,
  caller_tracking_link=NULL, incident_link=NULL, hospital_arrival=NULL, demo_started=FALSE,
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
