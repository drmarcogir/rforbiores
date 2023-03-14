#' Calculate structural diversity using hypervolumes
#'
#' @x = input dataframe

struc_div<-function(x){
  # only structural variables
  x %>%
    dplyr::select(rh98,rh50,rh75,rh25,pai,fhd_nrm,CanopyRatio)->x
  # calculate hypervolume
  hv = hypervolume_gaussian(x)
  # frich
  frich <- kernel.alpha(hv)
  # dispersion
  fdisp <- kernel.dispersion(hv)
  # evenness
  feven <- kernel.evenness(hv)
  res <- tibble(frich = frich, fdisp =  fdisp, feven = feven)
  return(res)
}
