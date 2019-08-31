make_data <- function() {
  source("R/utils.R")
  source("R/gis.R")           # bearing
  source("R/measurements.R")  # {uni1}_to_{unit2}

  warning(
    "This is a temporary script to fix some data issues found in STRIPSYield.
    The fix belongs to the actual data package."
  )

  # Read
  yield            <- STRIPSyield::yieldExtra

  # Transform to UTM
  proj4string      <- "+proj=longlat +datum=WGS84"
  proj4stringUTM   <- "+proj=utm +zone=15 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

  yield$latitude   <- yield$y
  yield$longitude  <- yield$x

  xyUTM            <- coordinateTransf(
    yield$x, yield$y, proj4string, proj4stringUTM
  )
  yield$x          <- xyUTM[, 1]
  yield$y          <- xyUTM[, 2]

  # Fix force timelapse
  yield$timelapse  <- round(yield$distance / (yield$speed / 60 / 60 * 5280), 1)

  # Compute yield
  yield$yieldDryBuAc   <- yield_equation(
    massFlowRate    = yield$flow,
    time            = yield$timelapse,
    distance        = yield$distance,
    swathWidth      = yield$swath,
    testMass        = grain_test_mass(yield$crop),
    harvestMoisture = yield$moisture,
    marketMoisture  = 0
  )

  yield$yieldMktBuAc   <- yield_equation(
    massFlowRate    = yield$flow,
    time            = yield$timelapse,
    distance        = yield$distance,
    swathWidth      = yield$swath,
    testMass        = grain_test_mass(yield$crop),
    harvestMoisture = yield$moisture,
    marketMoisture  = grain_market_moisture(yield$crop)
  )

  yield$yieldUnadjBuAc <- yield_equation(
    massFlowRate    = yield$flow,
    time            = yield$timelapse,
    distance        = yield$distance,
    swathWidth      = yield$swath,
    testMass        = grain_test_mass(yield$crop),
    harvestMoisture = yield$moisture,
    marketMoisture  = yield$moisture
  )

  # Rescale
  yield$swath     <- feet_to_meters(yield$swath)
  yield$elevation <- feet_to_meters(yield$elevation)
  yield$speed     <- mph_to_mps(yield$speed)
  yield$distance  <- feet_to_meters(yield$distance)
  yield$flow      <- lbs_to_kgs(yield$flow)

  # Augment
  yield$mass      <- yield$flow * yield$timelapse
  yield$area      <- yield$swath * yield$distance # m^2

  # Moisture adjustment
  yield$massOrig <- yield$mass # as collected
  yield$mass     <- yield$mass * (100 - yield$moisture) /
    (100 - grain_market_moisture(yield$crop)) # dry mass

  # Orbweaver splitup
  orbId          <- yield$site == "Orbweaver"
  orbYearSplit   <- split(
    yield[orbId, ],
    yield[orbId, ]$year
  )

  yield$siteOrig <- yield$site
  yield$site     <- as.character(yield$site)
  yield$site[orbId] <- do.call(
    c,
    lapply(
      orbYearSplit,
      function(orb) {
        pos  <- which.max(diff(orb$x)^2 + diff(orb$y)^2)
        lim  <- mean(c(orb$y[pos], orb$y[pos + 1]))
        site <- ifelse(orb$y > lim, "Orbweaver North", "Orbweaver South")
        site[pos] <- "Remove"
        site
      })
  )
  yield$site     <- as.factor(yield$site)
  yield          <- yield[yield$site != "Remove", ]

  # Time-ordered features should be run per yield and site
  yield$bearing   <- apply_ys(yield, function(df) {
    bearing(df$longitude, df$latitude) }
  )
  yield$haversine <- apply_ys(yield, function(df) {
    hav_distance_m(df$longitude, df$latitude) }
  )

  # Save
  save(yield, file = "./data/yield.rda", version = 2)
}
