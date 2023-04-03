######################################################################
#'  Function for preparing data for diversity calculations
#'  @ indf = path to csv file containing the data
#'####################################################################

div_master <- function(data,colname,splitf){

  #  Part 1 read in data
  dat<-fread(data)
  cat("\n still preparing the data... hang in there!")

  # subset column with grid name and get unique values
  dat %>%
    pull(colname) %>%
    unique() %>%
    # split into list
    split(ceiling(seq_along(.) / 500)) -> vl

  # split data into 10 files using the lookup list
  for (i in 1:length(vl)){
    tmp <- dat %>%
      filter(!!sym(colname) %in% vl[[i]])
    write_csv(tmp,paste0("./data/input_analyses/splitfiles/file_",i,".csv"))
  }
  #  Part 2 run calculations

}


