#' Create maps of functional traits
#'
#' @x = input dataframe


mapcreatecz<-function(x){
  countries %>%
    filter(NAME_LONG=="Czech Republic")->cz
  x %>%
    ggplot()+geom_raster(aes(x = x, y = y, fill = value))+geom_sf(data = cz,fill =NA)+
    theme_minimal()+scale_fill_viridis(limits=c(quantile(x$value,0.05,na.rm = TRUE),
                                                quantile(x$value,0.95,na.rm = TRUE)),
                                       oob = squish)+
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(hjust = 0.5,size = 25))+ggtitle(unique(x$title))->p
  fout<-paste0(unique(x$path),trimws(unique(x$title)),".png")
  ggsave(p,filename = fout,dpi = 400,width = 12, height = 8,
         bg="white")
  return(p)
}
