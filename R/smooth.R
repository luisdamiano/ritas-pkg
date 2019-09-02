smooth_polygons       <- function(spdf, formula, spdfPred = NULL,
                                  colIdentity = NULL, nCores = 1,
                                  type = "exact", ...) {
  f <- smooth_polygons_exact
  if (type == "frk")
    f <- smooth_polygons_frk

  f(spdf, formula, spdfPred, colIdentity, nCores, ...)
}

smooth_polygons_exact <- function(spdf, formula, spdfPred = NULL,
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

  # Split into groups
  nRow  <- nrow(spdfPred)
  ind   <- rep(1:nCores, each = ceiling(nRow / nCores))[1:nRow]
  spdfs <- sp::split(spdfPred, ind)

  # Start parallelization
  cl <- parallel::makeCluster(nCores, outfile = "")
  doParallel::registerDoParallel(cl)
  `%dopar%` <-
    if (nCores == 1) {
      foreach::`%do%`
    } else {
      foreach::`%dopar%`
    }
  l <- foreach::foreach(x = spdfs, .packages = c("sp", "gstat")) %dopar% {
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
  sp::proj4string(pred) <- sp::CRS(str)

  if (!is.null(colIdentity))
    pred@data[, colIdentity] <- spdf@data[, colIdentity]

  pred
}

smooth_polygons_frk   <- function(spdf, formula, spdfPred = NULL,
                                  colIdentity = NULL, nCores = 1, ...) {
  rhsVars  <- all.vars(formula[-2])

  # Create data spdf (remove covariates)
  spdfData <- spdf
  spdfData@data[, rhsVars]  <- NULL

  # Create prediction location spdfPred (remove others)
  if (is.null(spdfPred))
    spdfPred <- spdf

  spdfBAU  <- spdfPred
  spdfBAU@data[, !(colnames(spdfBAU@data) %in% rhsVars)] <- NULL

  # Fit & predict
  FRK::opts_FRK$set("parallel", nCores)
  fit      <- FRK::FRK(
    f         = formula,
    data      = spdfData,
    BAUs      = spdfBAU,
    vgm_model = gstat::vgm(NA, "Mat", NA, NA),
    regular   = 0, # 0 = irregularly placed basis functions
    # Go irregular as suggested in FRK vignette.
    ...
  )

  # Use obs_fs = FALSE: see analysisFineScaleVariation.R (article package)
  pred     <- FRK::predict(fit, obs_fs = FALSE)

  # Rename to homogenize with other smooth function
  colnames(pred@data)[which(colnames(pred@data) == "mu") ] <- "var1.pred"
  colnames(pred@data)[which(colnames(pred@data) == "var")] <- "var1.var"
  pred@data[, "sd"] <- NULL

  # Keep columns from aggregated spdf
  # if (!is.null(colIdentity))
  #   pred@data[, colIdentity] <- spdf@data[, colIdentity]

  if (!is.null(colIdentity))
    pred@data <-
      cbind(
        pred@data,
        do.call(
          rbind,
          lapply(1:nrow(pred), function(i) {
            colMeans(spdf[pred[i, ], ]@data[, colIdentity, drop = FALSE])
          })
        )
      )

  pred
}
