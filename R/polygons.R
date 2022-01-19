# Vehicle polygons --------------------------------------------------------

#' Find the vertices of the polygon associated with a moving vehicle.
#'
#' @param x A numeric vector with the latitude of the vehicle locations in UTM.
#' @param y A numeric vector with the longitude of the vehicle locations in UTM.
#' @param w A numeric vector with the width of the vehicle in meters.
#' @param d A numeric vector with the distance traveled by the vehicle in meters.
#' It must have the same length as the other vectors (the first value can be
#' a NA).
#' @return A matrix with eight columns with the coordinates of the four
#'   vertices, where 11 and 12 represent the points closer to the initial
#'   location and 21 and 22 represent the pointers closer to the final location.
#' @note `w` MUST BE IN METERS.
make_bounding_box <- function(x, y, w, d) {
  # Prepare centroids
  x0  <- c(NA, head(x, -1))
  x1  <- x
  y0  <- c(NA, head(y, -1))
  y1  <- y

  # Compute distance of the vertices to the centroid
  h   <- 0.5 * w # half width
  m   <- (y1 - y0) / (x1 - x0)
  dx  <- h / sqrt(1 + 1/m^2)
  dy  <- -dx / m

  # The slope of the perpendicular line doesn't exist when (y1 - y0) == 0
  # If it moved horizontally, the perpendicular line is vertical
  ind <- which(m == 0)
  dy[ind] <- h[ind]

  # Note: No need to fix for (x1 - x0) == 0
  # If it moved vertically, the perpendicular line is horizontal

  # Rescale the rectangle so that the diagonal has length d
  D   <- sqrt((x1 - x0)^2 + (y1 - y0)^2)
  d   <- ifelse(is.na(d), D, d)
  r   <- d / D

  x0  <- x0 + (1 - r) * (x1 - x0)
  y0  <- y0 + (1 - r) * (y1 - y0)

  # Compute vertices
  x01 <- x0 - dx
  y01 <- y0 - dy
  x02 <- x0 + dx
  y02 <- y0 + dy
  x11 <- x1 - dx
  y11 <- y1 - dy
  x12 <- x1 + dx
  y12 <- y1 + dy

  cbind(x0, y0, x1, y1, x01, y01, x02, y02, x11, y11, x12, y12)
}

#' Create polygons from a data.frame with centroid coordinates.
#'
#' Create a `\code{\link[sp]{SpatialPolygonsDataFrame}}` object from the `sp`
#' package based on the data.frame given as an input. For each row in the input
#' data.frame, a rectangle is constructed using the current and next positions
#' as well as the swath width.
#'
#' @param df A data.frame with columns `x`, `y`, and `swath`. The latter
#' represents the box width and must be in meters. Optionally, it can include
#' the column `d` with the box diagonal length  in meters.
#' @return A `\code{\link[sp]{SpatialPolygonsDataFrame}}` with one rectangle per
#' row.
#' @export
#' @note `swath`, and `d` if given, MUST BE IN METERS.
make_vehicle_polygons <- function(df, proj4string) {
  # Verify inputs
  if (!is.data.frame(df))
    stop("The input `df` must be a data.frame.")

  if (!all(c("x", "y", "swath") %in% colnames(df)))
    stop("The input `x` must be a data.frame with columns `x`, `y`, and `swath`.")

  if (!("d" %in% colnames(df)))
    df$d <- NA

  tryCatch(
    expr  = sp::CRS(proj4string),
    error = function(e) { stop(
        sprintf(
          "Invalid project4string: %s\nMessage:%s", proj4string, e$message
        )
    )}
  )

  # Compute vertices of the rectangles for each coordinate
  boxes <- apply_ys(df, function(xi) {
    make_bounding_box(xi$x, xi$y, xi$swath, xi$d)
  }, by = c("site", "year"))

  # Create a list with one Polygon per bounding box
  NAs <- apply(boxes, 1, anyNA)

  polygonList  <- apply(boxes[!NAs, ], 1, function(row) {
    list(
      sp::Polygon(
        cbind(
          rbind(row["x01"], row["x02"], row["x12"], row["x11"]),
          rbind(row["y01"], row["y02"], row["y12"], row["y11"])
        )
      )
    )
  })

  # Create a list with Polygons objects
  polygonsList <- lapply(seq_along(polygonList), function(i) {
    sp::Polygons(polygonList[[i]], ID = i)
  })

  # Create a SpatialDataFrame with Polygons and the data
  spatialPolygons <- sp::SpatialPolygons(
    Srl         = polygonsList,
    proj4string = sp::CRS(proj4string)
  )

  columns <- setdiff(
    colnames(df),
    c("x", "y")
  )

  data    <- df[!NAs, columns]
  rownames(data) <- 1:nrow(data)

  out <- sp::SpatialPolygonsDataFrame(
    spatialPolygons,
    data,
    match.ID = TRUE
  )

  out
}

# Reshape polygons --------------------------------------------------------

#' Crop out one polygone from another.
#'
#' @param spFrom A \code{\link[sp]{Polygon}} from which `polyTo` will be
#'   removed.
#' @param spTo A \code{\link[sp]{Polygon}} that will be cropped out of `spFrom`.
#' @return A \code{\link[sp]{Polygon}} object with the region of `spFrom` that
#'   is not within `spTo`.
crop_polygon <- function(spFrom, spTo) {
  if (!rgeos::gIsValid(spFrom, byid = TRUE))
    spFrom <- rgeos::gBuffer(spFrom, width = 0, byid = TRUE)

  if (!rgeos::gIsValid(spTo, byid = TRUE))
    spTo <- rgeos::gBuffer(spTo, width = 0, byid = TRUE)

  out <- rgeos::gDifference(
    spgeom1 = spFrom,
    spgeom2 = spTo,
    id      = sapply(spFrom@polygons, slot, name = "ID"),
    byid = TRUE
  )

  # TODO Consider buffering the output with width = 0 AND PLEASE USE BYID OKAY?

  out
}

#' Reshape overlapping polygons.
#' @param spdf A `\code{\link[sp]{SpatialPolygonsDataFrame}}` returned by
#' `\code{\link{make_vehicle_polygons}}`.
#' @param verbose If TRUE, progress will be printed.
#' @return The input object with reshaped polygons.
#' @export
reshape_polygons <- function(spdf, verbose = TRUE) {
  n     <- nrow(spdf)
  nLast <- n - 1

  tInit <- Sys.time()
  for (i in 1:nLast) {
    tryCatch({
      overPolygons   <- spdf[(i + 1):n, ][spdf[i, ], ]
      overPos        <- which(
        get_polygon_id(spdf) %in% get_polygon_id(overPolygons)
      )

      if (verbose)
        cat(
          sprintf(
            "\r[% 4i/% 4i |% 6.1f%% |% 5.2f minutes] Filtering out %3d overlapping polygons in this step.",
            i, nLast, 100 * i / nLast,
            difftime(Sys.time(), tInit, units = "mins"), length(overPos)
          )
        )

      for (j in overPos) {
        if (rgeos::gWithin(spdf[j, ], spdf[i, ])) {
          warning("reshape_polygons: skipped one polygon that was completely inside another.")
          next
        }

        cropped <- crop_polygon(spFrom = spdf[j, ], spTo = spdf[i, ])

        if (is.null(cropped)) {
          warning("reshape_polygons: the polygon is empty after reshaping.")
          next
        }

        spdf@polygons[j] <- cropped@polygons
      }
    },
    error = function(e) {
      warning(e)
    })
  }

  if (verbose)
    cat("\n")

  spdf <- rgeos::gBuffer(spdf, byid = TRUE, width = 0)
  spdf$effectiveArea <- get_polygon_area_m2(spdf)
  spdf
}

# Chop polygons -----------------------------------------------------------

#' Title
#'
#' @param spdf A `\code{\link[sp]{SpatialPolygonsDataFrame}}` with the polygons
#' that will be covered by the grid.
#' @param width Grid width in meters.
#' @param height Grid height in meters.
#' @return A data.frame with two columns.
make_grid_by_size <- function(spdf, width, height) {
  minmax  <- t(spdf@bbox)

  yStep   <- height
  yLength <- ceiling(abs((minmax[2, 2] - minmax[1, 2])) / yStep)
  ySeq    <- seq(from = minmax[1, 2], to = minmax[2, 2], length.out = yLength)

  xStep   <- width
  xLength <- ceiling(abs((minmax[2, 1] - minmax[1, 1]) / xStep))
  xSeq    <- seq(from = minmax[1, 1], to = minmax[2, 1], length.out = xLength)

  expand.grid(
    x1 = xSeq,
    x2 = ySeq
  )
}

#' Create a grid covering the given polygons.
#'
#' Note that `width` and `height` take priority over `n` if all are non-null.
#'
#' @param spdf A `\code{\link[sp]{SpatialPolygonsDataFrame}}` object with the
#'   polygons to be covered by the grid.
#' @param n The approximate number of polygons that the grid should contain.
#' @param width The width of the grid polygons in meters.
#' @param height The height of the grid polygons in meters.
#' @param minArea A numeric between 0 and 1 indicating the minimum proportion of
#'   area overlay between polygons in `spdf` and a pixel. Grid pixels whose area
#'   do not cover the minimum in proportion will be dropped.
#' @param regular If TRUE (default), the union of all the elements in the grid
#'   will form a regular polygons. If FALSE, grid polygons not intercepting with
#'   any of the input polygons will be dropped. NOTE: this is not about the
#'   elements of of the grid having a regular shape!
#' @return A `\code{\link[sp]{SpatialPolygonsDataFrame}}` object with the
#'   polygons conforming the grid.
#' @export
#' @references Loosely based on
#' https://ecosystems.psu.edu/research/labs/walter-lab/manual/chapter1/1.9-creating-a-square-polygon-grid-over-a-study-area
make_grid <-
  function(spdf, n = NULL, width = NULL, height = NULL, minArea = 0, regular = TRUE) {
  # Check
  if (is.null(n) & is.null(width) & is.null(height))
    stop("Either `n`, or `width` and `height`, must be non-null.")

  # Compute locations for a grid with n pixels
  if (!is.null(n))
    gridCoords  <- sp::makegrid(spdf, n)

  # Compute locations for a grid with height x width sized pixels
  if (!is.null(width) & !is.null(height))
    gridCoords  <- make_grid_by_size(spdf, width, height)

  # Create SP
  gridPoints  <- sp::SpatialPointsDataFrame(
    coords      = gridCoords,
    data        = gridCoords,
    proj4string = sp::CRS(sp::proj4string(spdf))
  )
  sp::gridded(gridPoints) <- TRUE
  gridPolygons <- as(gridPoints, "SpatialPolygons")

  out <- sp::SpatialPolygonsDataFrame(
    gridPolygons,
    data = data.frame(
      id = row.names(gridPolygons),
      row.names = row.names(gridPolygons)
    )
  )

  if (!regular)
    out <- out[spdf, ]

  if (minArea > 0) {
    spdfFix   <- rgeos::gBuffer(spdf, byid = TRUE, width = 0.1) # 10cm buffer
    spdfInd   <- rgeos::gIsValid(spdfFix, byid = TRUE)
    spdfUnion <- rgeos::gUnionCascaded(spdfFix[spdfInd, ])

    overlap   <- sapply(1:nrow(out), function(i) {
      get_polygon_area_m2(rgeos::gIntersection(spdfUnion, out[i, ])) /
        get_polygon_area_m2(out[i, ])
    })

    out <- out[overlap > minArea, ]
  }

  out
}

# Chop polygons -----------------------------------------------------------

#' Crops polygons using a grid.
#'
#' Chops many polygons into many finer pieces by ovelaying a grid.
#'
#' @param spdf A `\code{\link[sp]{SpatialPolygonsDataFrame}}` object with the
#'   polygons to be cropped.
#' @param gridSpdf A `\code{\link[sp]{SpatialPolygonsDataFrame}}` object with
#'   the grid as created by \code{\link{make_grid}}.
#' @param colIdentity A character vector with the name of the columns of `spdf`
#'   that should be passed through without modification.
#' @param colWeight A character vector with the name of the columns of `spdf`
#'   that should be multiplied by the areal weight (apportioning).
#' @param tol The tolerance value for simplifying the polygons before cropping
#'   (see \code{\link[sp]{gSimplify}}).
#' @return A `\code{\link[sp]{SpatialPolygons}}` list with the resulting
#'   polygons.
#' @export
chop_polygons <- function(spdf, gridSpdf, colIdentity, colWeight, tol = 1E-8) {
  overlays <- sp::over(spdf, gridSpdf, returnList = TRUE)
  l <-
    lapply(1:length(overlays), function(i) {
      lapply(1:nrow(overlays[[i]]), function(j) {
        tryCatch({
          gridPoly  <- gridSpdf[gridSpdf$id == overlays[[i]][j, ], ]
          innerPoly <- rgeos::gSimplify(spdf[i, ], tol)

          # If polygon is invalid, try to fix it
          if (!rgeos::gIsValid(innerPoly, byid = TRUE))
            innerPoly <- rgeos::gBuffer(innerPoly, width = 0, byid = TRUE)

          # If still invalid, drop it
          if (is.null(innerPoly))
            return(NULL)

          # Create resulting polygons
          outId   <- sprintf(
            "%s-%s", spdf[i, ]@data$record, gridPoly@polygons[[1]]@ID
          )
          outPoly <- rgeos::gIntersection(innerPoly, gridPoly, id = outId)
          weight  <- outPoly@polygons[[1]]@area / innerPoly@polygons[[1]]@area

          if (length(outPoly@polygons) > 1 || length(innerPoly@polygons) > 1)
            stop("something went wrong?")

          # Create resulting data.frame
          originalPolyID <- spdf[i, ]@data$record

          gridPolyID     <- gridPoly@polygons[[1]]@ID

          colIdentityDF  <- spdf[i, ]@data[, colIdentity]
          colnames(colIdentityDF) <- colIdentity

          colWADF        <- spdf[i, ]@data[, colWeight] * weight
          colnames(colWADF) <- sprintf("%sW", colWeight)

          outDF   <- cbind(
            originalPolyID,
            gridPolyID,
            colIdentityDF,
            colWADF,
            areaWeight = weight
          )
          rownames(outDF) <- outId

          # Create resulting spdf
          sp::SpatialPolygonsDataFrame(outPoly, outDF, match.ID = TRUE)
        }, error = function(e) {
          return(NULL)
        })
      })
    })

  # Filter out NULL and Spatial* objects not SpatialPolygons (e.g. lines)
  ul <- unlist(l)
  ul <- ul[sapply(ul, function(x) { class(x) == "SpatialPolygonsDataFrame" })]

  do.call(rbind, unlist(ul))
}

#' Aggregate the attributes of the polygons grouped by some criteria.
#'
#' @param spdf A `\code{\link[sp]{SpatialPolygonsDataFrame}}` object with the
#'   polygons to be cropped.
#' @param gridSpdf A `\code{\link[sp]{SpatialPolygonsDataFrame}}` object with
#'   the grid as created by \code{\link{make_grid}}.
#' @param by Character vector with the name of the columns used to group
#'   polygons.
#' @param minArea A numeric between 0 and 1 indicating the minimum proportion of
#'   area overlay between polygons in `spdf` and a pixel. Grid pixels whose area
#'   do not cover the minimum in proportion will be dropped.
#' @param colNames Character vector with the name of the columns that will be
#'   aggregated.
#' @param colFun Function vector with the function that will be applied to each
#'   column.
#' @return A `\code{\link[sp]{SpatialPolygonsDataFrame}}` object with the
#'   aggregated polygons.
#' @export
aggregate_polygons <-
  function(spdf, gridSpdf, by = NULL, minArea = 0, colNames, colFun) {
  # Check proportion range
  minArea <- max(min(minArea, 1), 0)

  # Create resulting data.frame
  l    <- lapply(
    1:length(colNames),
    function(i) {
      tapply(spdf@data[, colNames[i]], spdf@data[, by], colFun[[i]])
    }
  )
  outDF <- data.frame(l)
  colnames(outDF) <- colNames

  # Add upscaled values
  propUp <- mean(get_polygon_area_m2(gridSpdf)) / outDF$effectiveAreaW
  propDF <- outDF * propUp
  colnames(propDF) <- sprintf("%sUp", colNames)

  outDF <- cbind(
    outDF,
    propDF
  )

  # Drop unmatched pixels
  gridId <- get_polygon_id(gridSpdf)
  DFId   <- rownames(outDF)
  ind    <- gridId %in% DFId

  # Create resulting spdf
  outSP  <- sp::SpatialPolygonsDataFrame(
    gridSpdf[ind, ], outDF, match.ID = TRUE
  )

  # Drop pixels by area coverage
  ind    <- outSP$effectiveAreaW / get_polygon_area_m2(outSP) > minArea

  outSP[ind, ]
}

# Polygon utils -----------------------------------------------------------

#' Get ID of child polygons.
#'
#' @param sp A `\code{\link[sp]{Polygons}}` object.
#' @return A character vector with the ID of all the polygons conforming
#' the input object.
get_polygon_id <- function(sp) {
  sapply(
    slot(sp, "polygons"),
    function(sp) slot(sp, "ID")
  )
}

#' Get the area of child polygons.
#'
#' @param sp A `\code{\link[sp]{Polygons}}` object.
#' @return A numeric vector with the area in square meters of all the polygons
#' in the input object.
get_polygon_area_m2 <- function(spdf) {
  sapply(spdf@polygons, function(x) { x@area })
}
