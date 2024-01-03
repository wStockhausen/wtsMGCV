#' 
#' @title Plot all [mgcv::gam()] model smooth terms
#' @description Function to plot all [mgcv::gam()] model smooth term.
#' 
#' @param lstPredTermTbls - list of dataframes with predicted values for terms (or single dataframe)
#' @param labs - list of axis labels for covariates
#' @param dfrDat - original dataframe for gam model (for rug plots) or NULL (for no rug plots)
#' @param ori - orientation for stacking the 2D value/se plots ('H' or 'V')
#' 
#' @return list of ggplot2 plot objects (all of class gg)
#' 
#' @details \code{lstPredTermTbls} should be a list (of dataframes) from running [predSmoothTerms()]. 
#' \code{labs} should be a named list of 1-element character vectors, with names corresponding to 
#' term names and values the associated label to use. Currently, only 1D and 2D smooth terms can be 
#' plotted.
#' 
#' The intercept is added to the first model term, but not to subsequent terms.
#' 
#' @export 
#'
plotSmoothTerms<-function(lstPredTermTbls,labs,dfrDat=NULL,ori="H",ci=0.80){
  terms = names(lstPredTermTbls);
  vars  = getVarsForSmoothTerms(terms);
  lst = list();
  for (trm in terms){
    vrs = vars[[trm]];
    if (length(vrs)==1){
      lst[[trm]] = plotSmoothTerm1D(trm,lstPredTermTbls,dfrDat=dfrDat,xlab=labs[[vrs]],ci=ci);
    } else if (length(vrs)==2){
      lst[[trm]] = plotSmoothTerm2D(trm,lstPredTermTbls,dfrDat=dfrDat,xlab=labs[[vrs[1]]],ylab=labs[[vrs[2]]]);
    }
  }
  return(lst);
}
#--labs = list(z="size (mm CW)",d="depth (m)",t="temperature (deg C)",f="-log2(phi)",s="sorting");
#--ps = plotSmoothTerms(lstTermTbls,dfrDat,labs=labs);
