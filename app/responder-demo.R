# Deterministic, synthetic UI scenario. No routing, ETA model, or clinical advice.
responder_demo <- function(elapsed = 0, scenario = "Rain + rush hour") {
  stopifnot(length(elapsed) == 1L, is.finite(elapsed), elapsed >= 0)
  scenarios <- c("Rain + rush hour", "Dry + light traffic", "Snow/ice + road closure", "Heavy rain + rush hour")
  stopifnot(scenario %in% scenarios)
  list(
    incident_id = "DEMO-QNS-1042", unit = "Q-12", zone = "Astoria",
    location = "Synthetic scene near Astoria Park, Queens",
    briefing = "Reported fall; dispatcher requests EMS assessment (synthetic).",
    priority = "High — synthetic category", access = "Demo access point: park entrance on the east side. Confirm with dispatch.",
    elapsed = min(elapsed, 720),
    status = if (elapsed >= 720) "On scene" else if (elapsed >= 600) "Approaching scene" else if (elapsed >= 30) "En route" else "Assigned",
    scenario = scenario,
    weather_icon = switch(scenario, "Dry + light traffic" = "sun", "Snow/ice + road closure" = "snow", "Heavy rain + rush hour" = "heavy_rain", "rain"),
    weather = switch(scenario, "Rain + rush hour" = "Rain · moderate (synthetic)", "Heavy rain + rush hour" = "Heavy rain (synthetic)", "Dry + light traffic" = "Dry (synthetic)", "Snow/ice + road closure" = "Snow/ice (synthetic)"),
    traffic = switch(scenario, "Rain + rush hour" = "Heavy · simulated rush hour", "Heavy rain + rush hour" = "Heavy · simulated rush hour", "Dry + light traffic" = "Light · simulated", "Snow/ice + road closure" = "Closure reported · simulated"),
    tips = switch(scenario,
      "Heavy rain + rush hour" = c("Ask dispatch to verify accessible roads around the simulated congestion and any reported flooding.", "Confirm the incident entrance and caller access point before arrival.", "Ask dispatch for updated access restrictions; the route is not adjusted for rain or flooding."),
      "Rain + rush hour" = c("Ask dispatch to verify a viable alternate approach around simulated congestion.", "Confirm the entrance before arrival to reduce searching at the scene.", "Rain conditions: ask dispatch to confirm access restrictions; no faster-driving recommendation is implied."),
      "Dry + light traffic" = c("Confirm the exact incident location and entrance with dispatch.", "Ask dispatch to notify the caller of the agreed access point.", "Keep the briefing ready for the crew to reduce avoidable coordination delays."),
      "Snow/ice + road closure" = c("Ask dispatch to verify the reported closure and an accessible alternate approach.", "Confirm the scene entrance is accessible under the simulated winter conditions.", "Request an updated access briefing; weather context does not justify increased speed."))
  )
}


# Code-native vector icons with accessible labels; no third-party image assets.
weather_icon <- function(kind) {
  shape <- switch(kind,
    sun = '<g fill="none" stroke="#c58918" stroke-width="3" stroke-linecap="round"><circle cx="32" cy="29" r="10"/><path d="M32 9v5m0 30v5M12 29h5m30 0h5M18 15l4 4m20 20 4 4M18 43l4-4m20-20 4-4"/></g>',
    snow = '<path d="M16 35a10 10 0 0 1 1-20 14 14 0 0 1 27 1 10 10 0 0 1 3 19Z" fill="#d9e5ef" stroke="#617c94" stroke-width="2"/><g stroke="#4c91b8" stroke-width="2"><path d="M20 41v13m-6-9 12 5m0-5-12 5M42 41v13m-6-9 12 5m0-5-12 5"/></g>',
    heavy_rain = '<path d="M13 33a10 10 0 0 1 3-20 15 15 0 0 1 28 1 10 10 0 0 1 5 19Z" fill="#6e859b" stroke="#496075" stroke-width="2"/><g stroke="#2b7bb4" stroke-width="3" stroke-linecap="round"><path d="m17 39-4 9m15-9-4 9m15-9-4 9m15-9-4 9m-25 3-2 5m13-5-2 5m13-5-2 5"/></g>',
    '<path d="M14 34a10 10 0 0 1 3-20 14 14 0 0 1 27 1 10 10 0 0 1 4 19Z" fill="#d9e5ef" stroke="#617c94" stroke-width="2"/><g stroke="#2b7bb4" stroke-width="3" stroke-linecap="round"><path d="m21 41-3 8m15-8-3 8m15-8-3 8"/></g>')
  label <- switch(kind, sun="Dry conditions", snow="Snow or ice", heavy_rain="Heavy rain", "Rain")
  shiny::HTML(paste0('<svg width="64" height="64" viewBox="0 0 64 64" role="img" aria-label="',label,'">',shape,'</svg>'))
}
