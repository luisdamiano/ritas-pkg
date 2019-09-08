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
      # foreach::`%do%`
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
  sp::proj4string(spdf) <- sp::CRS(str)
  sp::proj4string(pred) <- sp::CRS(str)

  if (!is.null(colIdentity))
    pred@data[, colIdentity] <- aggregate(spdf, pred, FUN = mean)@data[, colIdentity]
    # pred@data[, colIdentity] <- spdf@data[, colIdentity]

  pred
}

smooth_polygons_frk   <- function(spdf, formula, spdfPred = NULL,
                                  colIdentity = NULL, nCores = 1, ...) {
  # Create locations
  spdfData <- spdf
  spdfBAUs <- spdfData

  if (is.null(spdfPred))
    spdfPred <- spdf

  # Create data spdf (remove covariates)
  lhsVars    <- all.vars(formula[[2]])
  rhsVars    <- all.vars(formula[[3]])

  if (length(lhsVars))
    spdfData@data[, !(colnames(spdfData@data) %in% lhsVars)] <- NULL

  if (length(rhsVars)) {
    spdfBAUs@data[, !(colnames(spdfBAUs@data) %in% rhsVars)] <- NULL
  } else {
    colnames(spdfBAUs@data) <- sprintf("%strash", colnames(spdfBAUs@data))
  }

  # Fit & predict
  FRK::opts_FRK$set("parallel", as.integer(nCores))
  fit      <- FRK::FRK(
    f         = formula,
    data      = spdfData,
    BAUs      = spdfBAUs,
    vgm_model = gstat::vgm(NA, "Mat", NA, NA),
    regular   = 0, # 0 = irregularly placed basis functions
    # Go irregular as suggested in FRK vignette.
    ...
  )

  # Use obs_fs = FALSE: see analysisFineScaleVariation.R (article package)
  pred     <- FRK::predict(fit, newdata = spdfPred, obs_fs = FALSE)

  # Rename to homogenize with other smooth function
  colnames(pred@data)[which(colnames(pred@data) == "mu") ] <- "var1.pred"
  colnames(pred@data)[which(colnames(pred@data) == "var")] <- "var1.var"
  pred@data[, "sd"] <- NULL

  # Keep columns from aggregated spdf
  if (!is.null(colIdentity))
    pred@data[, colIdentity] <- aggregate(spdf, pred, FUN = mean)@data[, colIdentity]

  pred
}
