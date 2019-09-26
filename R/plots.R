# Base plotting functions -------------------------------------------------
plot_canvas <- function(spdf, ...) {
  plot(
    spdf,
    border = "white",
    xlab = "",
    ylab = "",
    xaxt = "n",
    yaxt = "n",
    ...
  )
}

plot_polygon <- function(spdf, ...) {
  for (polys in spdf@polygons[spdf@plotOrder]) {
    for (poly in polys@Polygons[polys@plotOrder]) {
      polygon(poly@coords, ...)
    }
  }
}

plot_map <- function(spdf, main = NULL, subtitle = NULL, footer = NULL, backgroundArgs = NULL,
                     canvasArgs = NULL, pointArgs = NULL, polygonArgs = NULL,
                     mainArgs = NULL, subtitleArgs = NULL, legendArgs = NULL,
                     ...) {
  # Set up margins
  opar <- par(TRUE)
  par(mar = c(1.0, 0.5, 1.0, 0.5))

  # Draw new canvas
  do.call(plot_canvas, c(list(spdf), canvasArgs))

  # Draw background
  if (!is.null(backgroundArgs))
    do.call(add_background, c(list(spdf), backgroundArgs))

  # Draw points
  if (!is.null(pointArgs))
    do.call(points, c(list(x = sp::coordinates(spdf)), pointArgs))

  # Draw polygons
  if (!is.null(polygonArgs))
    do.call(plot, c(list(spdf, add = TRUE), polygonArgs))

  # Draw main title
  if (!(is.null(main) || main == "" || main == FALSE))
    do.call(
      mtext,
      c(
        list(
          text = main,
          side = 3,   # Top
          adj  = 0,   # Left alignment
          line = 0.1, # Margin between top line and text
          font = 2   # Bold,
        ),
        mainArgs
      )
    )

  # Draw subtitle
  if (!(is.null(subtitle) || subtitle == "" || subtitle == FALSE))
    do.call(
      mtext,
      c(
        list(
          text = subtitle,
          side = 3,   # Top
          adj  = 1,   # Right alignment
          line = 0.1, # Margin between top line and text
          font = 2    # Bold,
        ),
        subtitleArgs
      )
    )

  # Draw footer
  if (!(is.null(footer) || footer == "" || footer == FALSE))
    do.call(
      mtext,
      c(
        list(
          text = footer,
          side = 1,   # Top
          adj  = 0.5,   # Right alignment
          line = 0.1, # Margin between top line and text
          font = 2    # Bold,
        ),
        subtitleArgs
      )
    )

  # Draw legend
  if (!is.null(legendArgs))
    do.call(add_legend, c(list(spdf), legendArgs))

  invisible(par(opar))
}

add_background <- function(spdf, backgroundPath, backgroundCoords = NULL) {
  backImg   <- png::readPNG(backgroundPath)

  backPos   <- spdf@bbox[c(1, 3, 2, 4)]
  if (!is.null(backgroundCoords))
      backPos   <- backgroundCoords

  graphics::rasterImage(
    backImg,
    xleft   = backPos[1],
    ybottom = backPos[3],
    xright  = backPos[2],
    ytop    = backPos[4]
  )
}

add_legend <- function(spdf, colName, nGrad = 20, nTicks = 5, main = "",
                       textCol   = "black", tickCol = "black", lineCol = "black",
                       textCex   = 0.5,
                       boxOffset = 0,
                       ticksV    = TRUE, ticksQ = TRUE,
                       colFunQ   = interpolate_colors_empirical_q,
                       colFunInv = interpolate_colors_empirical_inv,
                       colDist   = spdf@data[, colName]) {
  # Extract value
  # x        <- spdf@data[, colName]
  x        <- colDist

  # Create gradient
  gradPos  <- seq(from = 0, to = 1, by = 1 / nGrad)
  gradCols <- colFunQ(gradPos, x)

  # Plot gradient
  # legPos   <- c(spdf@bbox[1, 2], spdf@bbox[2, 1])

  usr      <- graphics::par()$usr

  legImg   <- grDevices::as.raster(matrix(gradCols, nrow = 1))
  legPos   <- c(usr[2], usr[3])

  legBox   <- c(
    xleft   = legPos[1] - diff(usr[1:2]) / 5,
    ybottom = legPos[2],
    xright  = legPos[1],
    ytop    = legPos[2] + diff(usr[3:4]) / 40
  ) + boxOffset

  rasterImage(
    legImg,
    xleft   = legBox[1],
    ybottom = legBox[2],
    xright  = legBox[3],
    ytop    = legBox[4]
  )

  # Create ticks
  ticksQtl <- seq(from = 0, to = 1, length.out = nTicks)
  ticksVal <- colFunInv(ticksQtl, colDist)
  ticksPos <- seq(from = legBox[1], to = legBox[3], length.out = nTicks)

  # Plot ticks
  if (ticksQ) {
    graphics::points(
      x   = ticksPos,
      y   = rep(legBox[2], nTicks),
      pch = "|",
      cex = textCex,
      col = tickCol
    )

    graphics::lines(
      x   = ticksPos,
      y   = rep(legBox[2], nTicks),
      lwd = 0.5,
      col = lineCol
    )

    graphics::text(
      x = ticksPos,
      y = legBox[2],
      labels = sprintf("%.1f", ticksQtl),
      pos = 1,
      cex = textCex,
      col = textCol)
  }

  if (ticksV) {
    graphics::points(
      x   = ticksPos,
      y   = rep(legBox[4], nTicks),
      pch = "|",
      cex = textCex,
      col = tickCol
    )

    graphics::lines(
      x   = ticksPos,
      y   = rep(legBox[4], nTicks),
      lwd = 0.5,
      col = lineCol
    )

    graphics::text(
      x   = ticksPos,
      y   = legBox[4],
      labels = sprintf("%.1f", ticksVal),
      pos = 3,
      cex = textCex,
      col = textCol
    )
  }

  # lines(x = ticksPos, y = rep(legBox[2], nTicks), lwd = 0.5, col = lineCol)
  # lines(x = ticksPos, y = rep(legBox[4], nTicks), lwd = 0.5, col = lineCol)
  #
  # text(x = ticksPos, y = legBox[2], labels = sprintf("%.1f", ticksQtl), pos = 1, cex = textCex, col = textCol)
  # text(x = ticksPos, y = legBox[4], labels = sprintf("%.1f", ticksVal), pos = 3, cex = textCex, col = textCol)

  text(
    x = stats::median(ticksPos),
    y = legBox[4],
    labels = main,
    pos = 3,
    cex = textCex * 0.7 / 0.5,
    offset = 1.5,
    col = textCol
  )
}

#' Plot the vehicle in movement.
#'
#' @param sp A `spatialPolygons` object containing the vehicle polygons.
#' @param endpoints A data.frame containing the following columns:
#' x0, x1, y0, y1
#' @param main A character string with the plot title (optional).
#' @param subtitle A character string with the plot subtitle (optional).
#' @param cex The cex param.
#' @param boxes If TRUE, a rectangle representing the vehicle are drawn.
#' @param vertices If TRUE, points representing the vertices of the vehicle are drawn.
#' @param locations If TRUE, points representing the initial and final locations are drawn.
#' @param arrows If TRUE, arrows pointing from the initial to the final locations are drawn.
#' @param desc If TRUE, the bearing degrees and the distance in meters are printed.
#' @param legend If not NULL, the position of the legend.
#' @return Nothing.
plot_movement <- function(sp, endpoints, main = NA, subtitle = NA, cex = 1, boxes = TRUE, vertices = TRUE, locations = TRUE, arrows = TRUE, desc = TRUE, legend = "topright", ...) {
  opar <- par(TRUE)
  # par(mar = c(4.1, 4.1, 2.1, 2.1))

  d         <- euc_distance_m(sp::coordinates(sp)[, 1], sp::coordinates(sp)[, 2])
  v         <- do.call(rbind, lapply(sp[-1, ]@polygons, function(x) { x@Polygons[[1]]@coords }))

  plot(
    v,
    xlab = "",
    ylab = "",
    type = "p",
    pch  = 23,
    bg   = if (vertices) "darkgreen" else NA,
    col  = if (vertices) "darkgreen" else NA,
    mgp  = c(2.5, 1, 0), # Location of axis lab, tick lab, tick marks.
    cex  = cex,
    cex.axis = cex,
    cex.lab  = cex,
    xaxt = "n",
    yaxt = "n",
    bty  = 'no',
    ...
  )

  if (boxes)
    plot(
      sp[-1, ],
      col    = adjustcolor("lightgreen", alpha.f = 0.2),
      border = "darkgreen",
      lwd    = 1,
      lty    = 1,
      add = TRUE
    )

  if (locations)
    points(
      x   = endpoints$x0,
      y   = endpoints$y0,
      pch = 21,
      cex = cex,
      bg  = "black",
      col = "black"
    )

  if (arrows)
    arrows(
      endpoints$x0,
      endpoints$y0,
      endpoints$x1,
      endpoints$y1,
      len = 0.10,
      col = "black"
    )

  if (desc) {
    lagged <-
      if (is.na(endpoints$x0[1])) {
        2:nrow(sp)
      } else {
        1:nrow(sp)
      }

    text(
      x   = 0.5 * (endpoints$x0[lagged] + endpoints$x1[lagged]),
      y   = 0.5 * (endpoints$y0[lagged] + endpoints$y1[lagged]),
      labels = sprintf("%3.1fm", d[lagged]),
      adj    = c(0.5, 0.5), # Center horizontally and vertically,
      pos    = ifelse(endpoints$y0[lagged] < endpoints$y1[lagged], 4, 2),
      # adj    = c(-0.1, 0.5), # Align left (small margin), center vertically
      family = "mono",       # Help align the decimal points
      cex    = cex
    )
  }

  if (!(is.na(main) || main == "" || main == FALSE))
    mtext(
      text = main,
      side = 3,   # Top
      adj  = 0,   # Left alignment
      line = 0.1, # Margin between top line and text
      font = 2,   # Bold,
      cex  = cex
    )

  if (!(is.na(subtitle) || subtitle == "" || subtitle == FALSE))
    mtext(
      text = subtitle,
      side = 3,   # Top
      adj  = 1,   # Right alignment
      line = 0.1, # Margin between top line and text
      font = 2,   # Bold,
      cex  = cex
    )

  if (!(is.na(legend) || legend == "" || legend == FALSE))
    legend(
      x = legend,
      legend = c("Recorded position", "Computed vertices", "Spatial polygon", "Displacement vector"),
      col    = c("black"            , "darkgreen"        , NA               , "black"              ),
      fill   = c(NA                 , NA                 , adjustcolor("lightgreen", alpha.f = 0.2), NA),
      border = c(NA                 , NA                 , "darkgreen"      , NA                   ),
      lty    = c(NA                 , NA                 , NA               , 1                    ),
      pch    = c(21                 , 23                 , NA               , NA                   ),
      pt.bg  = c("black"            , "darkgreen"        , NA               , "black"              ),
      bty    = "n",
      seg.len = 0.5,
      cex    = 1
    )

  invisible(par(opar))
}
