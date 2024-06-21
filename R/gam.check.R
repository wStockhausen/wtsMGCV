#' 
#' @title Check a [mgcv::gam()] model (printout only)
#' @description Function to check a [mgcv::gam()] model (printout only). A port 
#' of [mgcv::gam.check()] that does not include the diagnostic plots.
#' 
#' @param b - a fitted gam object produced by [mgcv::gam()]
#' @param type - residuals type (see mgcv) 
#' @param k.sample - above this k uses random sub-sample of data 
#' @param k.rep number of reshuffles to do to get p-value for k-testing
#' 
#' @return character vector of output printed by [mgcv::gam.check()]
#' 
#' @details This function is a re-working of [mgcv::gam.check()] to provide 
#' a character vector output suitable for using, e.g., in a markdown document.
#' 
#' @import mgcv
#' 
#' @export 
#' 
gam.check<-function(
              b, 
              type = c("deviance", "pearson", "response"), 
              k.sample = 5000, 
              k.rep = 200){
    type <- match.arg(type)
    resid <- residuals(b, type = type)
    linpred <- if (is.matrix(b$linear.predictors) && !is.matrix(resid)) {
        napredict(b$na.action, b$linear.predictors[, 1])
    } else {napredict(b$na.action, b$linear.predictors)}
    
    gamm <- !(b$method %in% c("GCV", "GACV", "UBRE", "REML", 
                              "ML", "P-ML", "P-REML", "fREML", "NCV"))
    
    str = "";
    if (gamm) {
        str = paste0(str,"\n'gamm' based fit - care required with interpretation.");
        str = paste0(str,"\nChecks based on working residuals may be misleading.");
    } else {
        str = paste0(str,paste("\nMethod:", b$method, "  Optimizer:", paste(b$optimizer,collapse=" ")));
        if (!is.null(b$outer.info)) {
            if (b$optimizer[2] %in% c("newton", "bfgs")) {
                boi <- b$outer.info
                str = paste0(str,
                             paste("\n", paste(boi$conv,collapse=" "), " after ", boi$iter, " iteration", 
                                   sep = ""));
                if (boi$iter == 1){ 
                  str = paste0(str,".")
                } else str = paste0(str,"s.");
                str = paste0(str,paste("\nGradient range [", min(boi$grad), ",",max(boi$grad), "]", sep = ""));
                str = paste0(str,paste("\n(score ", b$gcv.ubre, " & scale ", b$sig2,").", sep = ""));
                ev <- eigen(boi$hess)$values
                if (min(ev) > 0){ 
                  str = paste0(str,paste("\nHessian positive definite, "))
                } else str = paste0(str,paste("\n"));
                str = paste0(str,paste("eigenvalue range [", min(ev), ",", max(ev),"].\n", sep = ""));
            }
            else {
                str = paste0(str,paste("\n"));
                str = paste0(str,"\n",paste(capture.output(print(b$outer.info)),collapse="\n"));
            }
        } else {
            if (length(b$sp) == 0) {
                str = paste0(str,paste("\nModel required no smoothing parameter selection"))
            } else {
                str = paste0(str,paste("\nSmoothing parameter selection converged after", 
                                       b$mgcv.conv$iter, "iteration"));
                if (b$mgcv.conv$iter > 1) 
                  str = paste0(str,paste("s"));
                if (!b$mgcv.conv$fully.converged) 
                  str = paste0(str,paste(" by steepest\ndescent step failure.\n"))
                else str = paste0(str,paste(".\n"));
                str = paste0(str,paste("The RMS", b$method, "score gradient at convergence was", 
                                       b$mgcv.conv$rms.grad, ".\n"));
                if (b$mgcv.conv$hess.pos.def) 
                  str = paste0(str,paste("The Hessian was positive definite.\n"))
                else str = paste0(str,paste("The Hessian was not positive definite.\n"))
            }
        }
        if (!is.null(b$rank)) {
            str = paste0(str,paste("Model rank = ", b$rank, "/", length(b$coefficients),"\n"));
        }
    }
    str = paste0(str,paste("\n"));
    kchck <- k.check(b, subsample = k.sample, n.rep = k.rep);
    if (!is.null(kchck)) {
        str = paste0(str,paste("Basis dimension (k) checking results. Low p-value (k-index<1) may\n"));
        str = paste0(str,paste("indicate that k is too low, especially if edf is close to k'.\n\n"));
        str = paste0(str,paste(capture.output(printCoefmat(kchck, digits = 3)),collapse="\n"));
    }
    return(str);
}
