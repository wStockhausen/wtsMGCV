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
