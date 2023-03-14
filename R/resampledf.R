#' Resample GEDI points within each square of a grid
#'
#' @x = input dataframe
#' @minpoints = minimum number of to sample


resampledf<-function(x,minpoints){
  if(dim(x)[1] > minpoints){
    res<- x[sample(1:dim(x)[1],minpoints),]
  } else {
    res <- x
  }
  return (res)
}
