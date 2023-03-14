#' Calculates dissimilarity between remote sensing predictors
#'
#' @x = input dataframe


diss_pred<-function(x){
  # average backscatter
  x %>%
    dplyr::select(!!varlookup %>%
                    filter(type == "S1 backscatter (average)") %>%
                    pull(variable)) %>%
    dist() %>%
    mean()->s1bck
  # s1 texture average
  x %>%
    dplyr::select(!!varlookup %>%
                    filter(type == "S1 texture (average)") %>%
                    pull(variable)) %>%
    mutate(across(where(is.numeric),rescale01)) %>%
    dist() %>%
    mean()->s1text
  # s1 seasonal backscatter
  x %>%
    dplyr::select(!!varlookup %>%
                    filter(type == "S1 backscatter (seasonal)") %>%
                    pull(variable)) %>%
    dist() %>%
    mean()->s1bckseas
  # s1 seasonal texture
  x %>%
    dplyr::select(!!varlookup %>%
                    filter(type == "S1 texture (seasonal)") %>%
                    pull(variable)) %>%
    mutate(across(where(is.numeric),rescale01)) %>%
    dist() %>%
    mean()->s1textseas
  # s2 spectral indices
  x %>%
    dplyr::select(!!varlookup %>%
                    filter(type == "s2 spectral index") %>%
                    pull(variable)) %>%
    mutate(across(where(is.numeric),rescale01)) %>%
    dist() %>%
    mean()->s2spec

  res <- tibble(s1bck,s1text,s1bckseas,s1textseas,s2spec)
  return(res)
}
