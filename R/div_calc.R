######################################################################
#'  Function for calculating diversity metrics
#'  The function takes the following arguments
#'  @ x =  a tibble::tibble with all the data records contained within a given
#'  grid cell
#'####################################################################

div_calc<-function(x,samples){

  # ------------------------------------ dissimilarity

  # euclidean distance
  x %>%
    # take random of sample of size n (300 but change!!!)
    # save selected data into temporary object called tmp
    dplyr::sample_n(samples) %>% {.->> tmp} %>%
    # select columns ending with _n (normalised structural variables)
    dplyr::select(ends_with("_n")) %>%
    # calculate euclidean distance matrix
    dist(diag = TRUE, upper = TRUE) %>%
    # take the mean of the eclu
    mean()->Eucl_beta

  # mean dissimilarity: horn-morisita index
  tmp %>%
    dplyr::select(ends_with("_n")) %>%
    vegan::vegdist(method = "horn",diag = TRUE,upper = TRUE) %>%
    mean()->Horn_beta
  # mean dissimilarity: Canberra
  tmp %>%
    dplyr::select(ends_with("_n")) %>%
    vegan::vegdist(method = "canberra",diag = TRUE,upper = TRUE) %>%
    mean()->Canberra_beta

  # mean dissimilarity: Bray
  tmp %>%
    dplyr::select(ends_with("_n")) %>%
    vegan::vegdist(method = "bray",diag = TRUE,upper = TRUE) %>%
    mean()->bray_beta

  # mean dissimilarity: kulczynski
  tmp %>%
    dplyr::select(ends_with("_n")) %>%
    vegan::vegdist(method = "kulczynski",diag = TRUE,upper = TRUE) %>%
    mean()->kulcz_beta

  # ------------------------------------ means and standard deviations
  tmp %>%
    dplyr::summarise_at(names(tmp)[4:13], sd, na.rm = TRUE) %>%
    dplyr::select_all(list(~ paste0("var_", .))) %>%
    dplyr::bind_cols(tmp %>%
                dplyr::summarise_at(names(tmp)[4:13], mean, na.rm = TRUE) %>%
                dplyr::select_all(list(~ paste0("avg_", .))))->str_univariate


  # ------------------------------------ variogram

  # convert data.frame into spatialpointsdataframe
  tmp1 = tmp
  sp::coordinates(tmp1) = ~x+y
  #tmp1 <- tibble::tibble(tmp1) %>% mutate(rh98 = rh98/10000)
  sp::proj4string(tmp1) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  tmp2 <- sp::spTransform(tmp1, sp::CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))
  #f <- try(automap::autofitVariogram(rh98 ~ 1, tmp2))
  Vario1 <- gstat::variogram(rh98 ~ 1, tmp2, cloud= FALSE)##,cutoff = 5000)#, )
  f <-  suppressMessages(try(gstat::fit.variogram(Vario1,  gstat::vgm(c("Exp", "Mat", "Sph")), fit.kappa = TRUE),silent = TRUE))
  if(inherits(f,"try-error")){
    sill <- NA
    range <-NA
    nugget <-NA
  } else {
    ## get fit parameters
    sill <- f$psill[2] # sill

    range <- f$range[2] # range

    nugget <- f$psill[1] # nugget
  }

  # ------------------------------------ Distance-decay

  # compute full distance matrix
   tmp %>%
     dplyr::select(ends_with("_n")) %>%
     vegan::vegdist(method = "bray",diag = TRUE,upper = TRUE) ->bray_beta_full
  # compute distance matrix for coordinates
   tmp %>%
     dplyr::select(x,y) %>%
     sf::st_as_sf(coords = c("x","y"),crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") %>%
     sf::st_transform(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs") %>%
     sf::st_coordinates() %>%
     dist()->spat_mat
  # final dataframe
   disdecay <- tibble::tibble(distance = as.numeric(spat_mat),
                      similarity = as.numeric(1-bray_beta_full))

   # quasibinomial glm
   mod<-try(glm(similarity~distance,data = disdecay,family = quasibinomial),silent = TRUE)
   if(inherits(mod,"try-error")){
     slope <- NA
   } else {
     slope <- coef(mod)[2]
   }


  #--------------------------------- convex hull
  #     # convex hull
  #tmp %>%
  #  dplyr::select(PC1,PC2,PC3)->tmp1
  #     area <- (convhulln(tmp1,options = "FA")$area*1000)
  #    vol <- (convhulln(tmp1,options = "FA")$vol*1000)

  #--------------------------------- traditional alpha indices
  # shannon diversity index
  tmp %>%
    dplyr::pull(H) %>%
    mean()->H_alpha

  # Simpson diversity index
  tmp %>%
    dplyr::pull(S) %>%
    mean()->S_alpha

  #--------------------------------- environmental data
  tmp %>%
    dplyr::summarise_at(names(tmp)[15:68], sd, na.rm = TRUE) %>%
    dplyr::select_all(list(~ paste0("var_", .))) %>%
    dplyr::bind_cols(tmp %>%
                dplyr::summarise_at(names(tmp)[15:68], mean, na.rm = TRUE) %>%
                dplyr::select_all(list(~ paste0("avg_", .))))->envdata

  #--------------------------------- put everything together
  #disdecay = list(disdecay)

  tibble::tibble(Eucl_beta,Horn_beta,Canberra_beta,bray_beta,kulcz_beta,
         H_alpha,S_alpha,data_samp = list(tmp)) %>%
    dplyr::bind_cols(envdata) %>%
    dplyr::bind_cols(str_univariate)->tmpres


  return(tmpres)
}


