#' Compute the yield in bushels per acre (bu/ac)
#'
#' @param massFlowRate mass flow rate (lb/sec)
#' @param time logging interval (sec)
#' @param distance distance traveled between logged data points (ft)
#' @param swathWidth header cut width (ft)
#' @param testMass grain density (lb/bu)
#' @param harvestMoisture moisture content (percentage)
#' @param marketMoisture market moisture content (percentage). Set to zero if you want the dry yield.
#' @return Yield (bu/ac)
#' @references Best Management Practices for Collecting AccurateYield Data and Avoiding Errors During Harvest http://extensionpublications.unl.edu/assets/pdf/ec2004.pdf
yield_equation <- function(massFlowRate, time, distance,
                           swathWidth, testMass,
                           harvestMoisture, marketMoisture = 0) {
  43560 * massFlowRate * time / distance / swathWidth / testMass *
    ((100 - harvestMoisture) / (100 - marketMoisture))
}

#' Return the test mass (weight) of a crop.
#'
#' @param cropString A character string, or a vector of character strings, with the name of the crop. Use either "Corn" or "Soybeans".
#' @return The test weight of the crop in pounds per bushel.
#' @examples grain_test_mass(c("Corn", "Soybeans"))
grain_test_mass <- function(cropString) {
  sapply(
    as.character(cropString),
    function(x) {
      switch(
        x,
        Corn     = 56,
        Soybeans = 60
      )
    }
  )
}

#' Return the nominal market moisture content of a crop.
#'
#' @param cropString A character string, or a vector of character strings, with the name of the crop. Use either "Corn" or "Soybeans".
#' @return The nominal market moisture content (% hundreds).
#' @examples grain_market_moisture(c("Corn", "Soybeans"))
grain_market_moisture <- function(cropString) {
  sapply(
    as.character(cropString),
    function(x) {
      switch(
        x,
        Corn     = 15.5,
        Soybeans = 13
      )
    }
  )
}

#' Compute the yield in megagrams per hectare (mg/ha)
#'
#' @param massKg mass (kg)
#' @param areaM2 area (square meters)
#' @return Yield (mg/ha)
yield_equation_mgha <- function(massKg, areaM2) {
  # 1 Kilogram     = 0.001  megagram (divide by  1000)
  # 1 square meter = 0.0001 hectare  (divide by 10000)
  # (massKg / 1000) / (areaM2 / 10000)
  10 * (massKg / areaM2)
}

#' Transform the parameter value from lognormal to normal distribution.
#'
#' @param mean A numeric with the mean of the lognormal distribution.
#' @param var A numeric with the variance of the lognormal distribution.
#' @param what A string with the name of the value to return. It can be `mean`,
#' `var`, or `median`.
#' @return A numeric with the transformed value.
#' @export
lognormal_to_normal <- function(mean, var, what) {
  if (what == "mean")
    return(exp(mean + 0.5 * var))

  if (what == "var")
    return(exp(2 * mean + var) * (exp(var) - 1))

  if (what == "median")
    return(exp(mean))

  stop("what should be `mean`, `var`, or `median`.")
}

# Conversions -------------------------------------------------------------
miles_to_kms    <- function(x) { x * 1.609 }
kms_to_miles    <- function(x) { x / 1.609 }

miles_to_feet   <- function(x) { x * 5280 }
feet_to_miles   <- function(x) { x / 5280 }

feet_to_meters  <- function(x) { x / 3.281 }
meters_to_feet  <- function(x) { x * 3.281 }

sqft_to_sqmt    <- function(x) { x / 10.7639 }
sqmt_to_sqft    <- function(x) { x * 10.7639 }

lbs_to_kgs      <- function(x) { x / 2.205 }
kgs_to_lbs      <- function(x) { x * 2.205 }

mph_to_mps      <- function(x) { 1000 / 3600 * miles_to_kms(x) }
mps_to_mph      <- function(x) { 1000 * 3600 / kms_to_miles(x) }

# https://www.extension.iastate.edu/agdm/wholefarm/pdf/c6-80.pdf
buac_to_kgha_corn    <- function(x) { x / 0.0159 }
kgha_to_buac_corn    <- function(x) { x * 0.0159 }
buac_to_kgha_soybean <- function(x) { x / 0.0149 }
kgha_to_buac_soybean <- function(x) { x * 0.0149 }

inches_to_feet  <- function(x) { x / 12 }
