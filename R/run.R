qlog <- function(x) {
  print(sprintf("[%s] %s", Sys.time(), x))
}

run_batch <- function(df, proj4string, gridArgs, predictArgs, finallyFun, colIdentity,
                      colWeight, colFun, name, resultsPath, imgPath, nCores) {
  if (!dir.exists(resultsPath))
    dir.create(resultsPath)

  if (!dir.exists(imgPath))
    dir.create(imgPath)

  qlog(sprintf("Start %s", name))

  qlog(sprintf("make_vehicle_polygons %s", name))
  poly1Out <- file.path(resultsPath, sprintf("%s_vehicle.RDS", name))
  poly1    <- make_vehicle_polygons(df, proj4string)
  saveRDS(poly1, poly1Out)

  qlog(sprintf("reshape_polygons %s", name))
  poly2Out <- file.path(resultsPath, sprintf("%s_reshaped.RDS", name))
  poly2    <- reshape_polygons(poly1, verbose = TRUE)
  saveRDS(poly2, poly2Out)

  # Start parallelization
  logOut   <- file.path(resultsPath, format(Sys.time(), "log_run_%Y%m%d_%H%M%S.txt"))
  cl       <- parallel::makeCluster(nCores, outfile = logOut)
  doParallel::registerDoParallel(cl)
  `%dopar%` <-
    if (nCores == 1) {
      foreach::`%do%`
    } else {
      foreach::`%dopar%`
    }

  foreach::foreach(
    g         = gridArgs,
    .packages = c("sp", "rgeos", "gstat"),
    .export   = ls(.GlobalEnv)
  ) %dopar% {
    key      <- paste(sprintf("%s%s_", names(g), unlist(g)), collapse = "")
    qlog(sprintf("Start %s %s", name, key))

    qlog(sprintf("make_grid %s %s", name, key))
    grid1Out <- file.path(resultsPath, sprintf("%s_grid_%s.RDS", name, key))
    grid     <- do.call(make_grid, c(list(poly2), g))
    saveRDS(grid, grid1Out)

    qlog(sprintf("chop_polygons %s %s", name, key))
    poly3Out <- file.path(resultsPath, sprintf("%s_chopped_%s.RDS", name, key))
    poly3    <- chop_polygons(poly2, grid, colIdentity, colWeight)
    saveRDS(poly3, poly3Out)

    qlog(sprintf("aggregate_polygons %s %s", name, key))
    poly4Out <- file.path(resultsPath, sprintf("%s_aggregated_%s.RDS", name, key))
    poly4    <- aggregate_polygons(
      poly3,
      grid,
      by       = "gridPolyID",
      colNames = sprintf("%sW", colWeight),
      colFun   = colFun
    )

    poly4$logMassW  <- log(poly4$massW)
    poly4$yieldMgHa <- yield_equation_mgha(poly4$massW, poly4$effectiveAreaW)

    saveRDS(poly4, poly4Out)

    # Predict
    qlog(sprintf("prepare_pred_locations %s %s", name, key))

    if ("nmax" %in% names(predictArgs) && predictArgs$nmax < 1) # from % to # of neighbors
        predictArgs$nmax <- round(nrow(poly4) * predictArgs$nmax)

    predictArgs$spdfPred <-
      if (is.null(predictArgs$location)) {
        # Use aggregated pixels
        poly4
      } else if (is.list(predictArgs$location)) {
        qlog(sprintf("create_pred_grid %s %s", name, key))
        # Construct grid
        predGrid      <- do.call(
          make_grid, c(list(poly4), predictArgs$location)
        )

        qlog(sprintf("create_pred_covariates %s %s", name, key))
        # Add attributes
        colNames      <- all.vars(predictArgs$formula[-2])
        predGrid@data <- cbind(
          predGrid@data,
          aggregate(
            x            = poly4[, colNames],
            by           = predGrid,
            FUN          = mean,
            areaWeighted = FALSE
          )@data
        )

        predGrid
      } else if ("SpatialPolygonsDataFrame" %in% class(predictArgs$location)) {
        # Use given locations
        predictArgs$location
    }

    qlog(sprintf("smooth_polygons %s %s", name, key))
    poly5Out <- file.path(resultsPath, sprintf("%s_smoothed_%s.RDS", name, key))
    poly5    <- do.call(
      smooth_polygons,
      c(list(poly4), predictArgs[-which(names(predictArgs) == "location")])
    ) # Remove location from the arg list

    # Transformations
    qlog(sprintf("Transformations %s %s", name, key))

    poly5$logMassKgMean <- poly5$var1.pred
    poly5$logMassKgVar  <- poly5$var1.var

    poly5$massKgMean    <- lognormal_to_normal(
      poly5$logMassKgMean, poly5$logMassKgVar, "mean")
    poly5$massKgVar     <- lognormal_to_normal(
      poly5$logMassKgMean, poly5$logMassKgVar, "var")

    poly5$yieldMgHaMean <- yield_equation_mgha(
      poly5$massKgMean, poly5$effectiveAreaW)
    poly5$yieldMgHaVar  <- yield_equation_mgha(
      1, poly5$effectiveAreaW)^2 * poly5$massKgVar

    saveRDS(poly5, poly5Out)

    qlog(sprintf("finally %s %s", name, key))
    if (is.function(finallyFun))
      finallyFun(poly1, poly2, poly3, poly4, poly5, name, resultsPath, imgPath)

    qlog(sprintf("Done with %s %s", name, key))
  } # End foreach

  qlog(sprintf("Done with %s", name))

  parallel::stopCluster(cl)
}
