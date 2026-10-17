# H-12: experimental ETA walkthrough

This demonstrates the existing H-11 synthetic assignment-to-scene model already
connected to the Responder and Caller under H-23. It predicts neither hospital
arrival nor GPS travel time. No new model, styling or baseline approval is added.

## Reproduce the complete example

From the repository root, with the app's R dependencies installed:

```sh
Rscript app/eta-view.R
```

`app/eta-view.R` provides an executable verification of the integrated view. The
live app already uses `shared_eta_card` from `app/shared-incidents.R`; a duplicate
UI module is unnecessary. The verifier sources the actual app into an isolated
environment and creates a separate in-memory incident store. It prints no private
tracking token, opens no server, changes no data files and needs no manual arrival.
The two incidents advance through the same `shared_advance` service used by the
responder replay, automatically stepping the simulated clock to scene arrival.

If your dependencies are installed in a separate R library, use your normal
`R_LIBS` setting. This development computer uses:

```sh
R_LIBS=/private/tmp/six-sigma-r-library Rscript app/eta-view.R
```

Expected output (whitespace may differ):

```text
EXPERIMENTAL ETA — SYNTHETIC DATA ONLY
Assignment: 2026-10-16 16:00:00 UTC; Astoria/High; Rain/Moderate; wind 10 mph; gust 16 mph
Median duration: 10.827923986 min; 95% individual prediction interval: [8.514285921, 13.770260822] min
Predicted scene arrival: 2026-10-16 16:10:49 UTC
Arrival range: 2026-10-16 16:08:30 UTC to 2026-10-16 16:13:46 UTC
PASS: independent model-matrix prediction and interval agree.
PASS: automatic replay ends at 12.0 min; original assignment prediction unchanged.
PASS: adding the completed record leaves all 15 core QC baseline limits unchanged.
Unavailable-input reason: missing or invalid wind
PASS: missing wind shows Not available yet; automatic arrival still completes; no zero estimate.
H-12 ETA VIEW PASS
```

The fixed timestamp represents noon EDT on October 16, 2026. The model is trained
on the frozen synthetic training split; this later example illustrates software
mechanics, not evidence of future operational performance. Clock strings display
whole seconds; numerical checks retain floating-point precision with explicit
seconds units and a one-microsecond timestamp tolerance.

## What is checked

1. The normal assignment creates the same stored prediction consumed by both
   role views, without using future arrival data.
2. An independent matrix calculation reconstructs the fitted log prediction
   `x beta`, its residual variance, and the individual prediction standard error
   `sqrt(sigma^2 + x Cov(beta) x')`. Exponentiating the center and its two-sided
   95% Student-t limits agrees with the ETA function. This checks an individual
   prediction interval, not a confidence interval for the mean. The displayed
   duration is the median on the original scale.
3. The replay finishes automatically at 12 simulated minutes. Its original ETA
   object remains identical; completion does not refit the model or retrospectively
   replace the estimate with the observed duration. The rendered card labels the
   estimate experimental/synthetic and distinguishes actual elapsed time.
4. Adding the completed shared record to the stable fixture leaves the fixed
   limits identical for every neighborhood/priority group. This check covers the
   call-to-scene adapter path; shared demo call receipt equals assignment. No
   hospital estimate or transport-baseline validation is implied.
5. A separate assignment has sustained wind deliberately missing. Its prediction
   is unavailable, its rendered card says `Not available yet`, and it still
   completes automatically. The estimate stays unavailable after completion;
   missing values do not become zero or a fabricated fallback.

## See it in the app

Run `Rscript app/testme.R`, open the local URL, and enter Primary responder with
the disposable demo credentials shown by the app. Select the Rain + rush hour
scenario and use the existing incoming-incident replay controls. Open that
incident's private caller link in another browser session. Both views use the
same frozen assignment prediction. Expand About this estimate to distinguish
predicted duration from replay elapsed time. Let the replay reach scene arrival;
the original estimate must remain unchanged. Destination confirmation can then
switch the caller map to the hospital route; the ETA remains for scene arrival.

Live examples use current assignment time, so their numerical values can differ
from the fixed example above. The unavailable-input example is reproducible with
the verifier; the current UI has no control for deleting weather measurements.
Do not claim a missing-weather button or manually edit production source to show
it. See [shared-incident walkthrough](shared-incidents.md) for role interaction.

## Supporting checks and limits

```sh
Rscript -e "testthat::test_file('tests/testthat/test-eta.R')"
Rscript tests/test-shared-incidents.R
```

The existing ETA tests independently cover known answers, model acceptance gates,
unsupported inputs, no-effect rejection and leakage. Shared integration tests
cover immediate caller updates and preserving the estimate through hospital
transport. See [H-11 review](../data/eta-model-review.md) for frozen periods,
acceptance thresholds, held-out results and assumptions. Synthetic validation
never validates actual EMS forecasts; no live weather or traffic service is used.

## Independent teammate reproduction completed

Isabel Wilmot ran `R_LIBS=/private/tmp/six-sigma-r-library Rscript app/eta-view.R`
in the integration checkout and supplied the complete terminal output on
2026-10-16. This tested the uncommitted H-12 files based on commit `ff8f26b`.

- Median duration matched: 10.827923986 minutes.
- Individual 95% prediction interval matched: [8.514285921, 13.770260822] minutes.
- Independent matrix calculation, automatic completion at 12 minutes and unchanged
  prediction/all 15 QC baseline limits passed.
- Missing-wind input remained unavailable while automatic arrival completed.
- Final output: `H-12 ETA VIEW PASS`.

This completes H-12's independent reproduction requirement. Synthetic model
verification does not constitute operational EMS validation.
