######################################################################
#'  Function for preparing data for diversity calculations
#'  @ flist = list of files to combine together
#'####################################################################

div_combine<-function(flist){
  # put everything into a single dataframe
  filel<-list.files(flist,full.names = TRUE)

  res <- NULL

  for(i in 1:length(filel)){
    readRDS(filel[i]) -> tmp
    tmp %>%
      dplyr::select(-c(data_samp)) %>%
      group_by(grid10km) %>%
      summarise_all(.,mean)->tmp1
    bind_rows(tmp1,res) -> res
  }
  return(res)
}

