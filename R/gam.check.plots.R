#' 
#' @title Diagnostic plots for a [mgcv::gam()] output object
#' @description Function to make diagnostic plots for a [mgcv::gam()] output object.
#' 
#' @param b - \pkg{mgcv} gam object 
#' @param type - type of residuals ("deviance","pearson","response")
#' @param rep - (default = 0)
#' @param level - (default = 0.9) 
#' @param s.rep - (default = 10)
#' @param combinePlots - flag (T/F) to combine plots (T) or return a list (F)
#' @param ... - not used
#' 
#' @return ggplot2 plot object (combinePlots=TRUE) or list of such (combinePlots=FALSE)
#' 
#' @details This function basically creates \pkg{ggplot2} versions of the plots provided by 
#' [mgcv::gam.check()].
#' 
#' @importFrom cowplot plot_grid
#' @importFrom wtsPlots getStdTheme
#' 
#' @import ggplot2
#' @import mgcv
#'
#' @export 
#' 
gam.check.plots<-function(
                   b, 
                   type = c("deviance", "pearson", "response"), 
                   rep = 0, 
                   level = 0.9, 
                   s.rep = 10,
                   combinePlots=TRUE,
                   ...){
    type <- match.arg(type)
    resid <- residuals(b, type = type)
    linpred <- if (is.matrix(b$linear.predictors) && !is.matrix(resid)) 
        napredict(b$na.action, b$linear.predictors[, 1])
    else napredict(b$na.action, b$linear.predictors)
    
    dfr = tibble::tibble(linpred=linpred,resid=resid);
    
    # if (is.null(.Platform$GUI) || .Platform$GUI != "RStudio") {
    #     old.par <- par(mfrow = c(2, 2));
    #     on.exit(par = old.par);
    # }
    
    # qq.gam(b, rep = rep, level = level, type = type, 
    #        rl.col = rl.col, rep.col = rep.col, ...);
    p0 = wtsMGCV::qq.gam(b,rep=rep,level=level,s.rep=s.rep)
    
    # plot(linpred, resid, main = "Resids vs. linear pred.", 
    #      xlab = "linear predictor", ylab = "residuals", ...);
    
    p1 = ggplot(dfr,aes(x=linpred,y=resid)) + 
           geom_point(alpha=0.4) + 
           geom_hline(yintercept=0, colour="red",linetype=2) +
           labs(x="linear predictor",y="residuals") + 
           wtsPlots::getStdTheme();
    
    # hist(resid, xlab = "Residuals", main = "Histogram of residuals",...);
    
    mn = mean(resid,na.rm=TRUE);
    sd = sd(resid,na.rm=TRUE);
    xs = mn+sd*seq(from=-4,to=4,by=0.05)
    pr = dnorm(xs,mn,sd)/dnorm(mn,mn,sd);
    p2 = ggplot(dfr,aes(x=resid,y=after_stat(ndensity))) + 
           geom_histogram(colour="black",fill="grey") + 
           geom_line(data=data.frame(x=xs,y=pr),aes(x=x,y=pr),colour="red",linetype=1,inherit.aes=FALSE) + 
           geom_vline(xintercept=mn,colour="red",linetype=2) + 
           labs(x="residuals",y="normalized density") + 
           wtsPlots::getStdTheme();
    
    fv <- if (inherits(b$family, "extended.family")){ 
            predict(b, type = "response")
          } else fitted(b);
    if (is.matrix(fv) && !is.matrix(b$y)) 
        fv <- fv[, 1];
    # plot(fv, napredict(b$na.action, b$y), xlab = "Fitted Values", 
    #     ylab = "Response", main = "Response vs. Fitted Values", 
    #     ...)
    dfr = data.frame(x=fv,y=napredict(b$na.action, b$y));
    p3 = ggplot(dfr,aes(x,y)) + 
           geom_point(alpha=0.4) + 
           geom_abline(slope=1,colour="red",linetype=2) + 
           labs(x="fitted values",y="response") + 
           wtsPlots::getStdTheme();
    
    gamm <- !(b$method %in% c("GCV", "GACV", "UBRE", "REML", 
        "ML", "P-ML", "P-REML", "fREML", "NCV"))
    if (gamm) {
        str = paste("\n'gamm' based fit - care required with interpretation.",
                    "\nChecks based on working residuals may be misleading.");
        warning(str);
    }
    
    if (combinePlots) {
      ps = cowplot::plot_grid(p0,p2,p1,p3,nrow=2,byrow=TRUE);
    } else {
      ps = list(qq=p0,resids_v_linpred=p1,resids_hist=p2,resp_v_fitted=p4);
    }
    return(ps)
}
