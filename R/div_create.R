######################################################################
#'  Function for preparing data for diversity calculations
#'  @ data = path to csv file containing the data
#'  @ colname = column name indicating grid cell size for aggregation
#'  @ size = chunk size (i.e. number of rows for each of the smaller dataframes)
#'####################################################################

div_create <- function(data,colname,size,thr){

  dat<-fread(data)

  dat %>%
    group_by_(colname) %>%
    summarise(count =n()) %>%
    filter(count >= thr) %>%
    dplyr::select(-c(count)) %>%
    inner_join(dat)->dat

  cat("\n still preparing the data... hang in there!")

  # subset column with grid name and get unique values
  dat %>%
    pull(colname) %>%
    unique() %>%
    # split into list (see size parameter)
    split(ceiling(seq_along(.) / size)) -> vl

  # split data into x number files using the lookup list
  for (i in 1:length(vl)){
    tmp <- dat %>%
      filter(!!sym(colname) %in% vl[[i]])
    write_csv(tmp,paste0("./data/input_analyses/splitfiles/file_",i,".csv"))
  }

}


