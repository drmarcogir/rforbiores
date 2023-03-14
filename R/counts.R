######################################################################
#'  Function for calculating number of GEDI observations within
#'  megapixels of different sizes. The function also calculates
#'  percentiles on the counts.
#'  The function takes the following arguments
#'  @ indf = gedi data.table, data.frame or tibble object
#'  @ x =  character vector indicating column name for grid
#'####################################################################

counts<-function(indf,x){
  indf %>%
    group_by_(x) %>%
    summarise(count = n())->tmp
  tibble(gridres = x) %>%
    bind_cols(as_tibble(t(as.matrix(quantile(tmp$count,
                                             probs=c(0.01,0.1,0.2,0.5,0.75,0.85,0.9,0.99))))))->res
  return(res)
}
