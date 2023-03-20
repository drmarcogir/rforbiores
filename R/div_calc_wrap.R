div_calc_wrap<-function(x){
  1:10 %>% map_dfr(\(i) div_calc(x))->res
  return(res)
}