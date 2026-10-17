# DIIS — Emergency Response Quality Monitor

DIIS connects a simulated ambulance journey with a quality-control dashboard.
Callers follow their ambulance and confirmed hospital destination; responders
manage the trip; supervisors investigate unusual completed response durations.
Built by team DIIS for the Cornell Six Sigma Hackathon, Health Systems —
Emergency Response challenge.

**Synthetic demonstration only.** No emergency request is sent. Results do not
measure real Queens EMS performance or establish clinical response targets.

## Open or run the app

**Hosted app:** [DIIS Emergency Response Monitor](https://connect.systems-apps.com/content/063ac41c-40a1-49bf-88dd-1ed5497a367f/)
on Posit Connect. Open **About this app** from the role-selection screen for a
plain-language overview and current development limits.

From a fresh clone, with R installed:

```sh
git clone https://github.com/xz1987/six-sigma.git
cd six-sigma
Rscript -e 'install.packages(c("shiny", "openssl", "jsonlite", "ggplot2", "testthat"), repos="https://cloud.r-project.org")'
Rscript app/testme.R
```

Open http://127.0.0.1:3838. Stop with Ctrl+C. Run commands from the repository
root. Optional environment variables: `PORT` changes the default 3838 port;
`HOST` changes the default `127.0.0.1` bind address. Internet is needed to install
packages and load map tiles. R 4.6 was used for local verification; other R
versions have not been independently verified.

Supervisor and Primary responder use disposable demo credentials displayed in
the login dialog. Caller tracking uses an expiring incident link and needs no
staff login. These are demonstration access controls.

## Follow one incident

1. Open Caller and create a demo incident. Retain its tracking link. Open
   Responder in another tab of the same app instance and log in.
2. Accept the waiting incident with the matching incident ID. Caller tracking
   changes from waiting for assignment to following the ambulance.
3. Advance the responder replay to scene arrival. The Supervisor receives the
   completed call-to-scene metric.
4. Select a hospital and confirm/begin transport. The Caller automatically shows
   that destination, its hospital marker and hospital route.
5. Complete the transport replay and confirm hospital arrival as the responder.
   The Supervisor receives the separate transport-to-hospital metric.

All tabs must connect to the same R process. Do not start independent servers for
the roles. [Shared workflow details](demo/shared-incidents.md).

## Demonstrate a supervisor's day

In Supervisor **Active tracking**, select a metric, zone and priority, then
**Try demo mode**. Choose **Normal day** or **Day with delays**, select
**Start demo day**, and reveal cases with **Next completed case** or
**Complete demo day**. Twelve synthetic cases illustrate a day; the delayed
scenario plants three final 30-minute durations. Counts, day-so-far average,
and I-MR charts update as cases complete.

The connected Caller/Responder trip remains visible alongside the graphs, with
its records kept separate from generated cases. Exiting demo mode discards those
cases and returns to the ordinary stream. Today's open day is excluded from
closed historical subgroups. Every Supervisor graph has a **How to read this
chart** button and a visible legend. Patterns and History analysis provide
separate descriptive and completed-period views.

Each of **Active tracking**, **Patterns** and **History analysis** ends its charts
with **What this view tells you**. Active tracking separates historical signals
from context-target misses and configured goals. Patterns explains medians,
sample counts and time-cell coverage; History distinguishes period averages
from individual-response p90 and service-goal performance. These summaries follow
their own view filters and do not infer causes.

History offers latest **7, 14, 30 or 90 calendar days** and **Full baseline year**,
with independent metric, zone, priority, records and subgroup-interval filters.
Automatic mode uses daily control-chart subgroups for short windows and weekly
ones for the full year. Its histogram always summarizes completed daily means.
A first-entry tutorial can be skipped or replayed with **Show page tutorial**.
Service goals start with explicitly synthetic suggestions and can be edited;
no intervention-tracking workflow is included.

## What the data contributes

The three [QC test datasets and codebook](data/README.md) provide a stable
scenario, planted delays and invalid/sparse/constant edge cases. The stable and
planted files share an identical baseline, so the demonstration tests whether
new delays are detected without moving the reference limits. They are alternative
scenarios and must not be concatenated. [Run the dataset demonstration](demo/README.md):

```sh
Rscript demo/run-datasets.R
```

Expected final output: `H-05 DATASET DEMO PASS`.

The separate [versioned ETA dataset](data/eta/README.md) supports an experimental
weather-aware prediction of assignment-to-scene duration. A predicted arrival
is different from an observed completed duration, and neither is a control limit.
The Caller clock refers to scene arrival; it is not a hospital-arrival prediction.
[Hospital sources and capabilities](data/hospitals/README.md) document the
reference directory; travel and vehicle movement remain simulated.

## Statistical method

The Supervisor analyzes **call received to scene arrival** and **transport
departure to hospital arrival** separately, in minutes, within five incident
zones and three priorities. Pending metric pairs are not completed observations.

For a fixed baseline, `sigma = mean(abs(diff(duration))) / 1.128`.
Individuals limits are `mean(duration) ± 3 × sigma`; moving-range limits are
zero and `3.267 × mean(abs(diff(duration)))`. Monitoring observations never refit
the baseline. The first monitoring range uses the last baseline observation in
the same group. See [measurement definitions and assumptions](SPEC.md) and the
[NIST method reference](https://www.itl.nist.gov/div898/handbook/pmc/section3/pmc322.htm).

Operational interpretation requires baseline review: at least 100 valid
observations, coverage of all 12 baseline months, nonzero variation and documented
suitability. Stability, skew, dependence and seasonality require review; sample
size alone does not approve a baseline. Synthetic review fixtures demonstrate
this workflow and are not operational approval. Additional SPC patterns request
investigation, not a diagnosis or proof of staff fault.

Medians, type-7 p90s, boxplots and heatmaps describe observations. Configured
service goals are management thresholds, separate from SPC limits. Adjusted
geometric means and their model intervals describe synthetic associations, not
causal effects or validated performance rankings. Closed daily/weekly means use
sample-count-dependent limits and are distinct from individual-incident charts.

## Verify and extend

[Developer reference](app/README.md) documents inputs, parameters, functions,
architecture and deployment. [Dataset demo](demo/README.md) explains expected
numbers; [test documentation](tests/README.md) explains numerical checks.

```sh
Rscript -e 'testthat::test_dir("tests", reporter="summary")'
Rscript -e 'testthat::test_dir("tests/testthat", reporter="summary")'
Rscript tests/verify-caller-responder-sessions.R
Rscript tests/verify-supervisor-daily-demo.R
Rscript tests/verify-deploy-bundle.R
```

Shared incident records and account state are process-local and reset on restart.
Hosted role synchronization requires a single R process. The app is not a durable
dispatch service, live GPS feed, clinical decision system, or validated routing
product. Automatic responder selection (H-15) is deferred. Offline installation
provides a fallback page; live interactions require a server connection.

Before submission, verify the documented hosted URL, ask a teammate to follow these
instructions from a fresh clone, and submit the public repository and demo on
Devpost. Deployment and submission remain separate verification tasks.

Event information remains in [schedule](docs/schedule.md),
[prompts](docs/prompts.md), [rubric](docs/criteria.md) and [resources](docs/resources.md).
