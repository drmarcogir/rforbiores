######################################################################
#'  Function for calculating  remaining data using a minimum number of
#'  observation per grid cell of various sizes. The grid cells are
#'  those used to bin GEDI data. The function also produces a map
#'  to verify the spatial coverage of the remaining observations
#'  when specific threshold is chosen.
#'  The function takes the following arguments
#'  @ indf = gedi data.table, data.frame or tibble object
#'  @ col =  character vector indicating column name for grid
#'####################################################################

thrsel<-function(df,col,thr,grid){
  df %>%
    group_by_(col) %>%
    summarise(count = n())->tmp
  # filter observations
  filtobsdf<- tmp %>%
    filter(count >= thr)
  filtobs<- filtobsdf %>%
    pull() %>%
    length()
  # create map
  filtobsdf %>%
    inner_join(grid)->finaldf
  finaldf %>%
    dplyr::select(x,y,count) %>%
    ggplot(aes(x=x,y=y,colour=count))+geom_point(size = 0.01)+xlim(-20,45)+
    scale_colour_distiller(palette ="Spectral",limits = c(quantile(finaldf$count,probs=c(0.01)),
                                                          quantile(finaldf$count,probs=c(0.95))),
                           oob=squish)+ggtitle(col)+
    theme(plot.title = element_text(size = 20))->p1
  # get percentage out of total
  tibble(grid = col,totobs = dim(tmp)[1],filtobs=filtobs,lookup = list(filtobsdf),
         plot = list(p1)) %>%
    mutate(percremain = (filtobs/totobs)*100)->res
  return(res)
}
