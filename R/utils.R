#' Apply a function for each combination of factors.
#'
#' @param X A data.frame with named columns including at least those in `by`.
#' @param FUN A function expecting a data.frame and returning an atom.
#' @param by A vector of character strings with the names of the columns used to split the dataset.
#' @return A vector with length equal to the number of rows in the data.frame containing the values returned by `FUN`.
#' @examples
#' \dontrun{
#'   apply_ys(yieldExtra, function(df) {
#'     bearing(df$x, df$y)
#'   })
#'   apply_ys(yieldExtra, function(df) {
#'     hav_distance_km(df$x, df$y)
#'   })
#' }
apply_ys <- function(X, FUN, by = c("year", "site")) {
  n    <- nrow(X)
  inds <- split(
    1:n,
    lapply(by, function(b) { X[, b] }),
    drop = TRUE
  )

  l <-  lapply(
    inds,
    function(ind) {
      FUN(X[ind, ][order(X[ind, ]$record), ])
    }
  )

  reduceFun <- rbind

  if (max(sapply(l, NCOL)) == 1) {
    reduceFun <- c
  }

  do.call(reduceFun, l)
}
