#' 
#' @title Plot a [mgcv::gam()] model smooth 2D term
#' @description Function to plot a [mgcv::gam()] model 2D smooth term.
#' 
#' @param term - name of smooth 2D term to plot
#' @param lstPredTermTbls - list of dataframes with predicted values for terms (or single dataframe)
#' @param dfrDat - original dataframe for gam model (for rug plots) or NULL (for no rug plots)
#' @param xlab - label for x axis 
#' @param yLab - label for y axis
#' @param ori - orientation for stacking the value/se plots ('H' or 'V')
#' @return ggplot2 plot object of class gg
#' 
#' @details lstPredTermTbls should be a list (of dataframes) from running [predSmoothTerms()].
#' 
#' @import ggplot2
#' 
#' @importFrom cowplot plot_grid
#' @importFrom rlang sym
#' 
#' @export 
#'
plotSmoothTerm2D<-function(term,lstPredTermTbls,dfrDat=NULL,xlab=NULL,ylab=NULL,ori="H"){
  if (inherits(lstPredTermTbls,"data.frame")) {
    dfrPred = lstPredTermTbls;
  } else dfrPrd = lstPredTermTbls[[term]];
  vars   = getVarsForSmoothTerms(term)[[1]];
  dci = (1-ci)/2;
  p1 = ggplot(dfrPrd,aes(x=!!rlang::sym(vars[1]),y=!!rlang::sym(vars[2]),z=value)) + geom_contour_filled() + 
         geom_point(aes(x=!!rlang::sym(vars[1]),y=!!rlang::sym(vars[2])),data=dfrDat,
                    colour="white",size=0.1,inherit.aes=FALSE) + 
          xlab(xlab)+ylab(ylab)+labs(subtitle=paste0(term," value"));
  p2 = ggplot(dfrPrd,aes(x=!!rlang::sym(vars[1]),y=!!rlang::sym(vars[2]),z=se)) + geom_contour_filled() + 
         geom_point(aes(x=!!rlang::sym(vars[1]),y=!!rlang::sym(vars[2])),data=dfrDat,
                    colour="white",size=0.1,inherit.aes=FALSE) + 
          xlab(xlab)+ylab(ylab)+labs(subtitle=paste0(term," se"));
  if (ori=="H"){
    pg = cowplot::plot_grid(p1,p2,nrow=1);
  } else {
    pg = cowplot::plot_grid(p1,p2,ncol=1);
  }
  return(pg);
}
#--plotSmoothTerm2D("ti(z,d)",lstTermTbls,dfrDat,xlab="size (mm CW)",ylab="depth (m)")
