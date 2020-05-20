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
