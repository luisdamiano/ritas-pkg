#' Compute the yield in megagrams per hectare (mg/ha)
#'
#' @param massKg mass (kg)
#' @param areaM2 area (square meters)
#' @return Yield (mg/ha)
yield_equation_mgha <- function(massKg, areaM2) {
  # 1 Kilogram     =  0.001 megagram (divide by  1000)
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
lognormal_to_normal <- function(mean, var, what) {
  if (what == "mean")
    return(exp(mean + 0.5 * var))

  if (what == "var")
    return(exp(2 * mean + var) * (exp(var) - 1))

  if (what == "median")
    return(exp(mean))

  stop("what should be `mean`, `var`, or `median`.")
}

#' Return the nominal market moisture content of a crop.
#'
#' @param cropString A character string, or a vector of character strings, with
#'   the name of the crop. Use either "Corn" or "Soybeans".
#' @return The nominal market moisture content (% hundreds).
##' @examples grain_market_moisture(c("Corn", "Soybeans"))
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
