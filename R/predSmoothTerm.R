#' 
#' @title Predict values for a smooth term
#' @description Function to predict values for a smooth term
#' 
#' @param model - [mgcv::gam()] model object
#' @param dfr - dataframe with covariates at which to predict the smooth term
#' @param term - name of the smooth term to do prediction for 
#' @param intercept - flag (T/F) to include intercept in returned values
#' 
#' @return tibble dataframe (tbl_df object)
#' 
#' @import mgcv 
#' 
#' @importFrom dplyr bind_cols
#' @importFrom dplyr mutate
#' 
#' @export
#' 
predSmoothTerm<-function(mdl,dfr,term,include_intercept=FALSE){
  prd = predict(mdl,dfr,
                type="iterms",terms=term,se.fit=TRUE,
                newdata.guaranteed=TRUE);
  dfrPrd = dplyr::bind_cols(dfr,
                            value=unname(prd$fit[,1]),
                            se=unname(prd$se.fit[,1]));
  if (include_intercept) {
    i = unname(mdl$coefficients[1]);#--intercept
    dfrPrd = dfrPrd |> dplyr::mutate(value=value+i);
  }
  return(dfrPrd);
}
