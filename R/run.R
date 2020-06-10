#' Title
#'
#' @param df
#' @param proj4string
#' @param site
#' @param year
#' @param resolution Can be a single numeric or a vector `c(5, 3, 1)`
#' @param nmax
#' @param nCores
#' @param filterOut
#' @param predictAt
#' @param folder
#'
#' @return
#' @export
#'
#' @examples
ritas <-
  function(df, proj4string, site = "unknownSite", year = "unknownYear",
           resolution = 1, nmax = 1, nCores = 1, filterOut = NULL,
           predictAt = NULL, folder = ".") {
    # Verify inputs
    if (!("data.frame" %in% class(df)))
      stop("The `df` argument must be a data frame")

    if (!all(c("x", "y", "swath", "mass") %in% colnames(df)))
      stop("The `df` data frame must have columns `x`, `y`, `swath`, and `mass`")

    if (!grep(glob2rx("*utm*"), tolower(proj4string)))
      stop("Coordinates should be in UTM format")

    # Autocomplete site and year if not present
    if (!("site" %in% colnames(df))) {
      warnStr <- "The `df` data frame has no `site` column, autocompleted with %s"
      logging::logwarn(warnStr, site)

      df$site <- site
    }

    if (!("year" %in% colnames(df))) {
      warnStr <- "The `df` data frame has no `year` column, autocompleted with %s"
      logging::logwarn(warnStr, year)

      df$year <- year
    }

    if (!("record" %in% colnames(df))) {
      warnStr <- "The `df` data frame has no `record` column, observation are assumed to be in temporal order"
      logging::logwarn(warnStr)

      df$record <- 1:nrow(df)
    }

    # Default values
    basePath    <- file.path(folder, site, year)
    colIdentity <- c(
      colnames(df)[!(colnames(df) %in% c("x", "y"))], # All columns except x, y
      "effectiveArea" # plus `effectiveArea`, which is created during processing
    )

    run_batch(
      df = df,
      proj4string = proj4string,
      gridArgs = lapply(resolution, function(r) {
        list(width = as.numeric(r), height = as.numeric(r), regular = FALSE)
      }),
      colIdentity = colIdentity,
      colWeight = c("mass", "effectiveArea"),
      colFun = c(sum, sum),
      predictArgs = list(
        spdf     = NULL,
        formula  = log(massWUp) ~ 1,
        spdfPred = NULL,
        nmax     = nmax,
        nCores   = nCores,
        colIdentity =
          c("massW", "effectiveAreaW", "massWUp", "effectiveAreaWUp")
      ),
      name        = tolower(sprintf("%s_%s", site, year)),
      resultsPath = file.path(basePath, "obj"),
      imgPath     = file.path(basePath, "img"),
      logPath     = basePath,
      nCores      = 1,
      finallyFun  = NULL,
      # finallyFun  = make_maps,
      filterOut   = filterOut
    )
  }

# TODO Change function name to `run`
# TODO Consider removing the outer loop and leave parallelization for smoothing
#      only
# TODO Allow running from a given step / cache do.calls
run_batch <-
  function(df, proj4string, gridArgs, predictArgs, finallyFun, colIdentity,
           colWeight, colFun, name, resultsPath, imgPath, logPath = NULL,
           nCores, filterOut = NULL) {
  # Create directories if they don't exist
  if (!is.null(logPath))
    logging::addHandler(
      logging::writeToFile,
      file = file.path(
        logPath,
        sprintf("log_%s.txt",format(Sys.time(), "%Y%m%d%H%M%S"))
      ),
      level = 0
    )

  if (!dir.exists(resultsPath))
    dir.create(resultsPath, recursive = TRUE)

  if (!dir.exists(imgPath))
    dir.create(imgPath, recursive = TRUE)

  # Setting info
  hr <- paste(rep("#", 80), collapse = "")
  logging::loginfo("%s", hr)
  logging::loginfo("RITAS settings for %s (see ?run_batch)", name)
  logging::loginfo(
    "df\t\tData frame with %d rows and %d columns (%s)",
    nrow(df),
    ncol(df),
    paste(colnames(df), collapse = ", ")
  )
  logging::loginfo("proj4string\t%s", proj4string)
  logging::loginfo("gridArgs\t%s", gridArgs)
  logging::loginfo("predictArgs\t%s", list(predictArgs))
  logging::loginfo("finallyFun\t%s", finallyFun)
  logging::loginfo("colIdentity\t%s", colIdentity)
  logging::loginfo("colWeight\t%s", colWeight)
  logging::loginfo("colFun\t%s", colFun)
  logging::loginfo("name\t\t%s", name)
  logging::loginfo("resultsPath\t%s", resultsPath)
  logging::loginfo("imgPath\t%s", imgPath)
  logging::loginfo("logPath\t%s", logPath)
  logging::loginfo("nCores\t%s", nCores)
  logging::loginfo("filterOut\t%s", filterOut)
  logging::loginfo("%s", hr)

  logging::logdebug("Start RITAS for %s", name)

  # Step 1: create rectangles from point data
  logging::logdebug("make_vehicle_polygons %s", name)

  poly1Out <-
    file.path(resultsPath, sprintf("%s_001_vehicle.RDS", name))
  poly1    <- make_vehicle_polygons(df, proj4string)
  saveRDS(poly1, poly1Out)

  if (!is.null(filterOut)) {
    logging::logdebug(
      "make_vehicle_polygons %s Filtering out %d polys",
      name, length(filterOut)
    )

    poly1Out <-
      file.path(resultsPath, sprintf("%s_001_vehicle_filtered.RDS", name))
    poly1    <- poly1[-filterOut, ]
    saveRDS(poly1, poly1Out)
  }

  # Step 2: clip polygons
  logging::logdebug("reshape_polygons %s", name)

  poly2Out <-
    file.path(resultsPath, sprintf("%s_002_reshaped.RDS", name))
  poly2    <- reshape_polygons(poly1, verbose = TRUE)
  saveRDS(poly2, poly2Out)

  # Start parallelization
  logOut   <- NULL
  # No parallelization logs
  #    file.path(basePath, format(Sys.time(), "log_run_%Y%m%d_%H%M%S.txt"))

  logging::logdebug("Make cluster with %d cores for %s", nCores, name)
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
    .packages = c("sp", "rgeos", "gstat")
  ) %dopar% {
    key      <- paste(sprintf("%s%s_", names(g), unlist(g)), collapse = "")
    logging::logdebug("Start processing %s at grid resolution %s", name, key)

    # Step 3: create a grid
    logging::logdebug("make_grid %s %s", name, key)

    grid1Out <-
      file.path(resultsPath, sprintf("%s_003_grid_%s.RDS", name, key))
    grid     <- do.call(make_grid, c(list(poly2), g))
    saveRDS(grid, grid1Out)

    # Step 4: break polygons into smaller pieces
    logging::logdebug("chop_polygons %s %s", name, key)

    poly3Out <-
      file.path(resultsPath, sprintf("%s_004_chopped_%s.RDS", name, key))
    poly3    <- chop_polygons(poly2, grid, colIdentity, colWeight)
    saveRDS(poly3, poly3Out)

    # Step 5: aggregate chopped polygons
    logging::logdebug("aggregate_polygons %s %s", name, key)

    poly4Out <-
      file.path(resultsPath, sprintf("%s_005_aggregated_%s.RDS", name, key))
    poly4    <- aggregate_polygons(
      poly3,
      grid,
      by       = "gridPolyID",
      colNames = sprintf("%sW", colWeight),
      colFun   = colFun
    )
    poly4$logMassW   <- log(poly4$massW)
    poly4$logMassWUp <- log(poly4$massWUp)
    saveRDS(poly4, poly4Out)

    # Step 6: smooth the aggregated polygons
    logging::logdebug("prepare_pred_locations %s %s", name, key)

    predictArgsLocal <- predictArgs
    # from % to # of neighbors
    if ("nmax" %in% names(predictArgsLocal) && predictArgsLocal$nmax < 1)
        predictArgsLocal$nmax <- round(nrow(poly4) * predictArgsLocal$nmax)

    # What to smooth
    predictArgsLocal$spdf <-
      if (is.null(predictArgsLocal$spdf) ||
          predictArgsLocal$spdf == "aggregated") {
        # Smooth aggregated pixels
        poly4
      } else if (predictArgsLocal$spdf == "original") {
        # Smooth data as collected
        # Ugly hack
        poly1$effectiveArea  <- get_polygon_area_m2(poly1)

        X <- poly1@data
        colnames(X) <- paste(colnames(X), "W", sep = "")
        poly1@data <- cbind(poly1@data, X)

        poly1
      } else {
        stop("I can only smooth the `aggregated` pixels or the `original` dataset.")
      }

    # Where to smooth at
    predictArgsLocal$spdfPred <-
      if (is.null(predictArgsLocal$spdfPred)) {
        # Use aggregated pixels
        poly4
      } else if (is.list(predictArgsLocal$spdfPred)) {
        # Construct grid
        logging::logdebug("create_pred_grid %s %s", name, key)

        predGrid      <- do.call(
          make_grid, c(list(poly4), predictArgsLocal$spdfPred)
        )

        # Add attributes (if none given, aggregate something that won't be used)
        logging::logdebug("create_pred_covariates %s %s", name, key)

        colNames      <- all.vars(predictArgsLocal$formula[-2])
        if (!length(colNames))
          colNames <- colnames(poly4@data)[1]

        predGrid@data <- cbind(
          predGrid@data,
          aggregate(
            x            = poly4[, colNames],
            by           = predGrid,
            FUN          = mean,
            areaWeighted = FALSE
          )@data
        )

        # Return only those pixels that overlay with the original sample
        predGrid[poly1, ]
      } else if ("SpatialPolygonsDataFrame" %in%
                 class(predictArgsLocal$spdfPred)) {
        # Use given locations
        predictArgsLocal$spdfPred
      }

    logging::logdebug("smooth_polygons %s %s", name, key)

    poly5Out <-
      file.path(
        resultsPath,
        sprintf(
          "%s_006_smoothed_%s_nmax%s.RDS",
          name,
          key,
          ifelse(
            "nmax" %in% names(predictArgs),
            predictArgs$nmax,
            "default"
          )
        )
      )
    poly5    <- do.call(
      smooth_polygons,
      predictArgsLocal
    )

    # Rename and add new values
    poly5$logMassKgMean <- poly5$var1.pred
    poly5$logMassKgVar  <- poly5$var1.var

    poly5$massKgMean    <- lognormal_to_normal(
      poly5$logMassKgMean, poly5$logMassKgVar, "mean")
    poly5$massKgVar     <- lognormal_to_normal(
      poly5$logMassKgMean, poly5$logMassKgVar, "var")

    poly5$yieldMgHaMean <- yield_equation_mgha(
      poly5$massKgMean, poly5$effectiveAreaWUp)
    poly5$yieldMgHaVar  <- yield_equation_mgha(
      1, poly5$effectiveAreaWUp)^2 * poly5$massKgVar

    saveRDS(poly5, poly5Out)

    # Step 7: post-processing
    if (is.function(finallyFun)) {
      logging::logdebug("Run %s %s %s", as.character(substitute(finallyFun)), name, key)
      finallyFun(poly1, poly2, poly3, poly4, poly5, name, resultsPath, imgPath)
    }

    logging::logdebug("Done processing %s at grid resolution %s", name, key)
  } # End foreach

  logging::logdebug("Stop cluster for %s", name)
  parallel::stopCluster(cl)

  logging::logdebug("Done with RITAS for %s", name)
}
