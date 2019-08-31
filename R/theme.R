colmin   <- adjustcolor("#e5f5e0", alpha.f = 1)
colmax   <- adjustcolor("#31a354", alpha.f = 1)

interpolate_colors_empirical     <- function(x, xMin = 0, qCap = 0.995, colmin = "#e5f5e0", colmax = "#31a354") {
  palette  <- colorRamp(c(colmin, colmax), alpha = TRUE)

  cap   <- quantile(x, qCap)
  xcap  <- pmin(x, cap)
  score <- ecdf(xcap)(xcap)
  cols  <- sapply(score, function(s) { rgb(palette(s) / 255) })
  cols
}

interpolate_colors_empirical_q   <- function(q, x, xMin = 0, qCap = 0.995, colmin = "#e5f5e0", colmax = "#31a354") {
  palette  <- colorRamp(c(colmin, colmax), alpha = TRUE)

  cap   <- quantile(x, qCap)
  xcap  <- pmin(x, cap)
  score <- ecdf(xcap)(quantile(xcap, probs = q))
  cols  <- sapply(score, function(s) { rgb(palette(s) / 255) })
  cols
}

interpolate_colors_empirical_red <- function(x, xMin = 0, qCap = 0.995, colmin = "#fee0d2", colmax = "#de2d26") {
  interpolate_colors_empirical(x, xMin, qCap, colmin, colmax)
}

interpolate_colors_empirical_q_red <- function(q, x, xMin = 0, qCap = 0.995, colmin = "#fee0d2", colmax = "#de2d26") {
  interpolate_colors_empirical_q(q, x, xMin, qCap, colmin, colmax)
}

interpolate_colors_empirical_inv <- function(q, x, xMin = 0, qCap = 0.995) {
  cap   <- quantile(x, qCap)
  xcap  <- pmin(x, cap)
  quantile(xcap, q)
}

# interpolate_colors <- function(x, xMin = 0, qCap = 0.995) {
#   cap   <- quantile(x, qCap)
#   score <- pmin((x - xMin) / (cap - xMin), 1)
#   cols  <- sapply(score, function(s) { rgb(palette(s) / 255) })
#   cols
# }
#
# interpolate_colors_normal <- function(x, xMin = 0, qCap = 0.995) {
#   cap   <- quantile(x, qCap)
#   xcap  <- pmin(x, cap)
#   score <- pnorm((xcap - mean(xcap)) / sd(xcap))
#   cols  <- sapply(score, function(s) { rgb(palette(s) / 255) })
#   cols
# }
