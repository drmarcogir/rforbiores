######################################################################
#'  Function for preparing data for diversity calculations
#'  @ flist = list of files to combine together
#'####################################################################

div_combine<-function(filel){

  res <- NULL
  failed <- NULL

  for (i in seq_along(filel)) {
    print(i)

    # Wrap the loop body in a tryCatch block
    tryCatch({
      result <- readRDS(filel[i])

      result %>%
        dplyr::select(-c(data_samp)) %>%
        group_by(grid10km) %>%
        summarise_all(.,mean) -> tmp1

      bind_rows(tmp1,res) -> res

    }, error = function(e) {
      # Handle the error here
      cat("Error reading file", filel[i], ":", conditionMessage(e), "\n")
      failed <<- bind_rows(failed, tibble(file = filel[i], status = "failed"))
    })
  }

  finalres <- tibble(res = list(res),failed = list(failed))

  return(finalres)
}

