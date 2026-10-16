# Emergency Response Quality Monitor

Supervisor interface scaffold built from `demos/shinyapp/app.R`.

From the repository root, run `Rscript app/testme.R`, then open
http://127.0.0.1:5300. Requires R and the `shiny` and `openssl` packages.

Upload a CSV containing `incident_id`, `zone`, `call_priority`,
`dispatch_time`, and `on_scene_time`. The interface checks the column
names and previews records, with zone and priority filters and a CSV export.
Full record validation, date filtering, response calculations, control charts,
and investigation flags are pending H-03. Preview rows are not validated incidents.
Uploads remain in the current Shiny session; no database or live dispatch feed exists.

Baseline date controls capture proposed settings only. No statistical result is
displayed until the separate statistical engine is implemented and verified.

The welcome screen routes to Supervisor, Primary responder, or Person calling
for EMS. Supervisor opens the CSV dashboard; responder and caller views are
explicit prototype placeholders. Change role returns to the welcome screen.
Supervisor and Primary responder require their matching demo accounts. Caller needs no login. No EMS requests are sent.

Demo usernames are `supervisor.demo` and `responder.demo`. Disposable passwords
are generated in memory when the application starts and displayed in the login
dialog for hackathon reviewers. Password verification uses salted bcrypt-PBKDF.
These shared demo accounts are for synthetic data only; they are not production
authentication. Five failed attempts trigger a 30-second per-session delay.
Logging out reloads the page, ending staff access and clearing uploaded data.
