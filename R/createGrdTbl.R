#'
#' @title Create a \pkg{tibble}/dataframe by expanding a set of grids
#' @description Function to create a tibble/dataframe by expanding a set of grids.
#' 
#' @param ... - named list of 1D grids or name/grid pairs
#' @return \pkg{tibble} object of class 'tbl_df' with outer cross of all 1D grids
#' 
#' @details The string "grd_" is stripped from any column name in the output
#'  dataframe.
#'  
#' @importFrom tidyr expand_grid
#' @importFrom rlang dots_list  
#' @importFrom rlang !!!
#' @importFrom stringr str_remove
#' 
#' @export
#' 
createGridTbl<-function(...){
  n = ...length();
  cat("length = ",n,"\n")
  if ((n==1) && (is.list(...elt(1)))){
    #--grids given as a list
    lst = ...elt(1);
  } else {
    #--grids given as ...
    lst = rlang::dots_list(...,.named=TRUE);
  }
  cat("nms = ",names(lst),"\n")
  dfr = tidyr::expand_grid(!!!lst);
  names(dfr) = stringr::str_remove(names(lst),"grd_");
  return(dfr);
}
