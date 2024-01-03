#'
#' @title Get the names of the smooth terms in a [mgcv::gam()] model
#' @description Function to get the names of the smooth terms in a gam model.
#' 
#' @param model - the [mgcv::gam()] gam model
#' @return a character vector with the names of the smooth terms
#' 
#' @importFrom stringr str_remove
#' 
#' @export
#' 
getSmoothTerms<-function(model){
  nms = names(model$sp);
  unms = unique(stringr::str_remove(nms,"[^)]*$"));
  return(unms);
}

