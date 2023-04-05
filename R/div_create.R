######################################################################
#'  Function for preparing data for diversity calculations
#'  @ data = path to csv file containing the data
#'  @ colname = column name indicating grid cell size for aggregation
#'  @ size = chunk size (i.e. number of rows for each of the smaller dataframes)
#'####################################################################

div_create <- function(data,colname,size){

  dat<-fread(data)
  cat("\n still preparing the data... hang in there!")

  # subset column with grid name and get unique values
  dat %>%
    pull(colname) %>%
    unique() %>%
    # split into list (see size parameter)
    split(ceiling(seq_along(.) / size)) -> vl
}


