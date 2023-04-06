######################################################################
#'  Function for copying files over
#'####################################################################


file_cp<-function(){
  # delete all  old files
  unlink("./data/input_analyses/splitfiles_tmp/*")

  # Use str_replace_all to change the extension to CSV in the file name
  csv_file_names <- str_replace_all(basename(file_list), "\\.rds$", ".csv")

  # Create the source file paths by concatenating the directory path with the CSV file names
  src_files <- paste0("./data/input_analyses/splitfiles/", csv_file_names)

  # Create the destination file paths by replacing the extension of the original file names with CSV and prepending the destination directory path
  dst_files <- file.path("./data/input_analyses/splitfiles_tmp", str_replace(basename(file_list), "\\.rds$", ".csv"))

  # Use purrr::walk2 to copy files from source to destination
  walk2(src_files, dst_files, file.copy)
}

