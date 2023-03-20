
div_calc<-function(x){

  # ------------------------------------ dissimilarity
  
  # euclidean distance
  x %>%
    # take random of sample of size n (300 but change!!!)
    # save selected data into temporary object called tmp
    sample_n(300) %>% {.->> tmp} %>%
    # select columns ending with _n (normalised structural variables)
    dplyr::select(ends_with("_n")) %>%
    # calculate euclidean distance matrix
    dist(diag = TRUE, upper = TRUE) %>%
    # take the mean of the eclu
    mean()->Eucl_beta
  
  # mean dissimilarity: horn-morisita index
  tmp %>%
    dplyr::select(ends_with("_n")) %>%
    vegdist(method = "horn",diag = TRUE,upper = TRUE) %>%
    mean()->Horn_beta
  # mean dissimilarity: Canberra
  tmp %>%
    dplyr::select(ends_with("_n")) %>%
    vegdist(method = "canberra",diag = TRUE,upper = TRUE) %>%
    mean()->Canberra_beta
  
  # mean dissimilarity: Bray
  tmp %>%
    dplyr::select(ends_with("_n")) %>%
    vegdist(method = "bray",diag = TRUE,upper = TRUE) %>%
    mean()->bray_beta
  
  # mean dissimilarity: kulczynski
  tmp %>%
    dplyr::select(ends_with("_n")) %>%
    vegdist(method = "kulczynski",diag = TRUE,upper = TRUE) %>%
    mean()->kulcz_beta
  
  # ------------------------------------ means and standard deviations
  
  
  # ------------------------------------ variogram
  
  # convert data.frame into spatialpointsdataframe
  tmp1 = tmp
  coordinates(tmp1) = ~x+y
  #tmp1 <- tibble(tmp1) %>% mutate(rh98 = rh98/10000)
  proj4string(tmp1) <- CRS("EPSG:4326")
  tmp2 <- spTransform(tmp1, sp::CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))
  f <- try(autofitVariogram(rh98 ~ 1, tmp2))
  #Vario1 <- gstat::variogram(rh98 ~ 1, input2, cloud= FALSE)##,cutoff = 5000)#, )
  #f <-  fit.variogram(Varioold,  vgm(c("Exp", "Mat", "Sph")), fit.kappa = TRUE)
  
  ## Look at the result of the fit
  # f
  sill <- f$var_model$psill[2] # sill
  
  range <- f$var_model$range[2] # range
  
  nugget <- f$var_model$psill[1] # nugget
  
  # sill
  vario_res<-tibble(sill,range,nugget)
  
  
  # ------------------------------------ Distance-decay
  
  # compute full distance matrix  
  tmp %>%
    dplyr::select(ends_with("_n")) %>%
    vegdist(method = "bray",diag = TRUE,upper = TRUE) ->bray_beta_full
  # compute distance matrix for coordinates
  tmp %>%
    select(x,y) %>%
    st_as_sf(coords = c("x","y"),crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") %>%
    st_transform(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs") %>%
    st_coordinates() %>%
    dist()->spat_mat
  # final dataframe 
  disdecay <- tibble(distance = as.numeric(spat_mat),
                     similarity = as.numeric(1-bray_beta_full))
  
  #--------------------------------- convex hull
  #     # convex hull
  #tmp %>%
  #  dplyr::select(PC1,PC2,PC3)->tmp1
  #     area <- (convhulln(tmp1,options = "FA")$area*1000)
  #    vol <- (convhulln(tmp1,options = "FA")$vol*1000)
  
  #--------------------------------- traditional alpha indices
  # shannon diversity index
  tmp %>%
    pull(H) %>%
    mean()->H_alpha
  
  # Simpson diversity index
  tmp %>%
    pull(S) %>%
    mean()->S_alpha
  
  #--------------------------------- environmental data
  tmp %>%
    summarise_at(names(tmp)[13:59], sd, na.rm = TRUE) %>%
    dplyr::select_all(list(~ paste0("var_", .))) %>%
    bind_cols(tmp %>%
                summarise_at(names(tmp)[13:59], sd, na.rm = TRUE) %>%
                dplyr::select_all(list(~ paste0("avg_", .))))->envdata
  
  #--------------------------------- put everything together
  tibble(Eucl_beta,Horn_beta,Canberra_beta,bray_beta,kulcz_beta,sill,range,nugget,
         disdecay = list(disdecay),H_alpha,S_alpha,data_samp = list(tmp)) %>%
    bind_cols(envdata) ->tmpres
  
  
  return(tmpres)
}
