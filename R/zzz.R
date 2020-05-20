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

}
