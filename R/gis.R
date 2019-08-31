# Transform from and to original CRS
coordinateTransf <- function(x, y, proj4stringIn, proj4stringOut) {
  isNA <- is.na(x[1])

  p    <- sp::SpatialPoints(
    cbind(na.omit(x), na.omit(y)),
    proj4string = sp::CRS(proj4stringIn)
  )
  p    <- sp::spTransform(p, sp::CRS(proj4stringOut))

  if (isNA)
    return(rbind(c(NA, NA), sp::coordinates(p)))

  sp::coordinates(p)
}

#' Compute the distance between two coordinates in meters
#'
#' @param x A numeric vector with longitudes.
#' @param y A numeric vector with latitudes.
#' @return A numeric vector with the distance in meters
#' @author Jaap https://stackoverflow.com/a/23095329
hav_distance_m <- function(x, y) {
  x0 <- head(x, -1)
  x1 <- tail(x, -1)
  y0 <- head(y, -1)
  y1 <- tail(y, -1)

  h <- geosphere::distHaversine(
    cbind(x0, y0),
    cbind(x1, y1)
  )

  c(NA, h)
}

#' Compute the distance between two coordinates in meters
#'
#' @param x A numeric vector with easting position in meters.
#' @param y A numeric vector with northing position in meters.
#' @return A numeric vector with the distance in meters
euc_distance_m <- function(x, y) {
  x0 <- head(x, -1)
  x1 <- tail(x, -1)
  y0 <- head(y, -1)
  y1 <- tail(y, -1)

  h  <- sqrt((x1 - x0)^2 + (y1 - y0)^2)

  c(NA, h)
}
