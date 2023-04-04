######################################################################
#'  Function for preparing data for diversity calculations
#'  @ indf = path to csv file containing the data
#'####################################################################

div_master <- function(data,colname,splitf,size){

  #  Part 1 read in data
  dat<-fread(data)
  cat("\n still preparing the data... hang in there!")

  # subset column with grid name and get unique values
  dat %>%
    pull(colname) %>%
    unique() %>%
    # split into list
    split(ceiling(seq_along(.) / size)) -> vl

  # split data into 10 files using the lookup list
  for (i in 1:length(vl)){
    tmp <- dat %>%
      filter(!!sym(colname) %in% vl[[i]])
    write_csv(tmp,paste0("./data/input_analyses/splitfiles/file_",i,".csv"))
  }
  #  Part 2 run calculations (remember to change path!)
  system("./code/bash/diversity_calc.sh ./data/input_analyses/splitfiles ./data/input_analyses/splitfiles_rds")

  # Part 3: put everything into a single dataframe
  filel<-list.files("./data/input_analyses/splitfiles_rds/",full.names = TRUE)

  res <- NULL

  for(i in 1:length(filel)){
    readRDS(filel[i]) -> tmp
    tmp %>%
      dplyr::select(-c(data_samp)) %>%
      group_by(grid10km) %>%
      summarise_all(.,mean)->tmp1
    bind_rows(tmp1,res) -> res
  }

}


