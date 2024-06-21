#' 
#' @title Plot a [mgcv::gam()] model smooth 1D term
#' @description Function to plot a [mgcv::gam()] model 1D smooth term.
#' 
#' @param term - name of smooth 1D term to plot
#' @param lstPredTermTbls - list of dataframes with predicted values for terms (or single dataframe)
#' @param dfrDat - original dataframe for gam model (for rug plots) or NULL (for no rug plots)
#' @param xlab - label for x axis 
#' @param ci - tow-sided confidence interval to plot
#' 
#' @return ggplot2 plot object of class gg
#' 
#' @details lstPredTermTbls should be a list (of dataframes) from running [predSmoothTerms()].
#' 
#' @import ggplot2
#' 
#' @importFrom rlang sym 
#' @importFrom wtsPlots getStdTheme
#' 
#' @export 
#'
plotSmoothTerm1D<-function(term,lstPredTermTbls,dfrDat=NULL,xlab=NULL,ci=0.80){
  dfrPrd = lstPredTermTbls[[term]];
  vars   = getVarsForSmoothTerms(term)[[1]];
  dci = (1-ci)/2;
  p = ggplot(dfrPrd,aes(x=!!rlang::sym(vars[1]),y=value,ymin=qnorm(dci,value,se),ymax=qnorm(1-dci,value,se))) + 
        geom_line() + geom_ribbon(alpha=0.2) + 
        xlab(xlab) + ylab(term) + 
        wtsPlots::getStdTheme();
  if (!is.null(dfrDat))      
    p = p+ geom_rug(aes(x=!!rlang::sym(vars[1]),y=0),data=dfrDat,position=position_jitter(height=0),
                    inherit.aes=FALSE,alpha=0.2);
  return(p);
}
#--plotSmoothTerm1D("ti(z)",lstTermTbls,dfrDat,xlab="size (mm CW)")


