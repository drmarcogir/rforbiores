#' Helper function for creating an irregular sequence of numbers
#' @inlist = input list to be processed
#' @cpu = number of cpus to be used
#'
#'

div_master<-function(inlist,cpu){
  # generate sequence (to loop through)
  SeqFinal <- seqlast(from=0,to=length(inlist),by=1000)
  # list where to store results
  mylist <- list()
  # loop through sequence
  for (i in 1:(length(SeqFinal)-1)){
    plan(multisession, workers = cpu)
    future_lapply(inlist[(SeqFinal[i]+1):SeqFinal[i+1]],div_calc_wrap,future.seed=TRUE)->res
    mylist <- append(mylist,res)
    # close session (change plan)
    plan(sequential)
  }
  return(mylist)
}

