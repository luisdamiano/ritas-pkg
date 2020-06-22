smooth_polygons <- function(spdf, formula, spdfPred = NULL,
                                  colIdentity = NULL, nCores = 1, ...) {
  if (is.null(spdfPred))
    spdfPred <- spdf

  str  <- sp::proj4string(spdf)
  sp::proj4string(spdf)     <- sp::CRS(as.character(NA))
  sp::proj4string(spdfPred) <- sp::CRS(as.character(NA))

  # Fit variogram
  vgm  <- gstat::variogram(formula, spdf)
  fit  <- gstat::fit.variogram(
    vgm,
    model      = gstat::vgm(NA, "Mat", NA, NA),
    fit.sills  = TRUE,
    fit.ranges = TRUE,
    fit.kappa  = TRUE
  )

  logging::loginfo(
    "Fitted variogram: %s",
    paste(capture.output(dput(fit, control = "niceNames")), collapse = "")
  )

  # Split into groups
  nRow  <- nrow(spdfPred)
  ind   <- rep(1:nCores, each = ceiling(nRow / nCores))[1:nRow]
  spdfs <- sp::split(spdfPred, ind)

  # Start parallelization
  if (!is.na(parallel::detectCores()))
    nCores <- min(nCores, parallel::detectCores())

  cl <- parallel::makeCluster(nCores, outfile = "")
  doParallel::registerDoParallel(cl)
  `%dopar%` <-
    if (nCores == 1) {
      foreach::`%do%`
    } else {
      # foreach::`%do%`
      foreach::`%dopar%`
    }
  l <-
    foreach::foreach(x = spdfs, .packages = c("sp", "gstat")) %dopar% {
      gstat::krige(
        formula   = formula,
        locations = spdf,
        newdata   = x,
        model     = fit,
        debug.level = -1,
        ...
      )
  }
  parallel::stopCluster(cl)

  pred <- do.call(rbind, l)
  sp::proj4string(spdf) <- sp::CRS(str)
  sp::proj4string(pred) <- sp::CRS(str)

  if (!is.null(colIdentity))
    pred@data[, colIdentity] <-
      aggregate(spdf, pred, FUN = mean)@data[, colIdentity]

  pred
}
