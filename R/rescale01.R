#' Normalises variables of a dataframe(by column)
#'
#' @x = input dataframe

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
