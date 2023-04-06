######################################################################
#'  Function for preparing data for diversity calculations
#'  @ csv = list of csv files
#'  @ rds = list of rds files
#'####################################################################

miss_list<-function(csv,rds){
  tibble(filen = list.files(csv)) %>%
    mutate(csv_name = filen,filen = str_remove(filen,'.csv')) %>%
    anti_join(tibble(filen = list.files(rds)) %>%
                mutate(filen = gsub("output_|\\.rds", "", filen)))
  pull(csv_name)
}


