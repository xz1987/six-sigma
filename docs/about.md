# About the DIIS Emergency Response Monitor

The DIIS Emergency Response Monitor is a hackathon prototype for exploring how
incident-level response-time data can help supervisors find unusual delays that
monthly averages may hide. It brings a synthetic caller, responder, and
supervisor workflow together with statistical process-control views.

**This is a demonstration only.** It does not contact 911 or EMS, dispatch an
ambulance, provide clinical advice, or provide operational navigation. Use 911
for an actual emergency in the United States.

## How to use the demo

Start at the role picker:

- **Caller:** Create a synthetic tracking link. It is a local demo action; no
  message or emergency request is sent. The private link expires after 30
  minutes.
- **Primary responder:** Sign in with the disposable account shown in the login
  dialog. Accept the queued caller incident or start a responder demo, advance
  through scene arrival, review the hospital options for the scenario, and
  confirm the hospital trip and arrival.
- **Supervisor:** Sign in with the disposable account shown in the login dialog.
  Review the shared stream in Active tracking, descriptive Patterns, or History
  analysis. Every section includes a **What this view tells you** summary after
  its charts, with reading guides and legends. **Try daily simulation** reveals a normal
  or delayed 12-case day. History has a separate **Try yearly simulation** with
  one-day, seven-day or full-2026 progression, independent filters and a
  skippable tutorial. Daily, yearly and ordinary live-trip records stay separate;
  Patterns has no replay controls.

The staff accounts are generated when the app process starts. The caller,
responder, and supervisor views share one in-memory synthetic incident while
the server process is running. A server restart clears active demo state; the
hosted instance is configured for one process so the role views share that
state.

## What is implemented

- Incident-level Individuals and Moving Range control-chart monitoring for
  call-received-to-scene-arrival time and transport-departure-to-hospital-arrival
  time, analyzed separately by zone and simulated call priority.
- Supporting supervisor summaries, investigation signals, and descriptive
  views. A signal is a request to investigate; it does not establish a cause,
  clinical outcome, or staff fault.
- One synthetic incident shared across the role views, with explicit scene and
  hospital arrival confirmations.
- A Queens demonstration map with five selected zones, stored road-route
  snapshots, and a directory of real hospitals and published service details.
  Hospital availability is not checked.
- Weather and traffic coordination prompts for the responder scenario. They
  are examples, not live conditions or measured time savings.

## Development status and limits

The deployed prototype uses invented incidents and deterministic bundled
fixtures. Its response-time records, hospital routes, weather and traffic
conditions, responder locations, and scenario clocks are not live operational
feeds. The statistical views demonstrate the analysis on documented synthetic
data; they are not evidence about actual Queens EMS performance. Experimental
arrival estimates are not dispatch guidance.

The app has no connection to a computer-aided dispatch system, no durable event
database, no live GPS, no live traffic or weather feed, and no verified
hospital-capacity feed. The shared incident state is held in the running R
process and resets when that process restarts. Moving toward operational use
would require appropriate EMS system integration, secure identity and audit
controls, durable storage, validation with domain experts, and safety and
privacy review. These are future development needs, not current features.

## Explore and run the project

- [App setup, modules, and data details](../app/README.md)
- [Synthetic dataset demonstration](../demo/README.md)
- [Project specification](../SPEC.md)
- [Live demo](https://connect.systems-apps.com/content/063ac41c-40a1-49bf-88dd-1ed5497a367f/)

Run locally from the repository root with `Rscript app/testme.R`, then open
`http://127.0.0.1:3838`. The app requires R packages `shiny`, `openssl`,
`jsonlite`, and `ggplot2`.
