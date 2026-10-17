# Run from repository root. Never writes H-02 core fixtures.
source('data/eta/fixture.R')
x <- eta_write_fixture('data/eta/v1')
cat(sprintf('Generated eta-v1: weather_effect=%d, no_weather_effect=%d, truth=%d rows; hourly weather observations=%d.\n',
 nrow(x$weather_effect),nrow(x$no_weather_effect),nrow(x$truth),nrow(x$weather_observations)))
