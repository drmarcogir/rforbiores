######################################################################
#'  Wrapper function for repeating diversity calculations 10 times
#'  The function takes the following arguments
#'  @ x =  a tibble with all the data records contained within a given
#'  grid cell
#'####################################################################

div_calc_wrap <- function(x, samples) {
  1:10 %>%
    map_dfr(~ div_calc(x, samples)) -> res
  return(res)
}





