#'
#' @title Get the covariate names for the smooth terms in a [mgcv::gam()] model
#' @description Function to get the covariate names for the smooth terms in a gam model.
#' 
#' @param terms - character vector with the names of smooth terms from a [mgcv::gam()] gam model
#' @return a named list with a character vector with the names of the covariates for each smooth term
#' 
#' @details The names of the list elements are the terms for which the covariate names were determined.
#' 
#' 
#' @importFrom stringr str_extract
#' @importFrom stringr str_split
#' 
#' @export
#' 
getVarsForSmoothTerms<-function(terms){
  #--strip everything after ), including )
  lst = terms |> 
          stringr::str_extract("(?<=\\().+?(?=\\))") |> 
          stringr::str_split(",",simplify=FALSE);
  names(lst) = terms;
  return(lst);
}

