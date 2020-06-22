#' @details
#' See the Introduction vignette: vignette("introduction", package = "yieldMaps")
#' Additionally, you may start with the manual help for ?ritas.
#' @keywords internal
"_PACKAGE"

.onAttach <- function(libname, pkgname) {
  # Set up logger
  logging::basicConfig(level = 0)
}

.onLoad <- function(libname, pkgname) {
  useCache <- tryCatch(
    expr  = { !is.null(asNamespace("memoise")) },
    error = function(e) { FALSE }
  )

  if (useCache) {
    # Default cache filesystem
    cache <- function(f) {
      memoise::memoise(
        f     = f,
        cache = memoise::cache_filesystem("./.cache")
      )
    }

    # Memoise these functions only
    # Example from https://github.com/r-lib/memoise/blob/master/R/memoise.R
    make_vehicle_polygons <<- cache(make_vehicle_polygons)
    reshape_polygons      <<- cache(reshape_polygons)
    make_grid             <<- cache(make_grid)
    chop_polygons         <<- cache(chop_polygons)
    aggregate_polygons    <<- cache(aggregate_polygons)
    smooth_polygons       <<- cache(smooth_polygons)
  }
}
