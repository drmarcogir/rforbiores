#' Helper function for creating an irregular sequrence of numbers
#' @from = starting value
#' @to = ending value
#' @by = increment of the sequence
#'
seqlast <- function (from, to, by) {
  vec <- do.call(what = seq, args = list(from, to, by))
  if ( tail(vec, 1) != to ) {
    return(c(vec, to))
  } else {
    return(vec)
  }
}
