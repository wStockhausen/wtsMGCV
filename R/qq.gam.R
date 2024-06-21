#' 
#' @title QQ plot for [mgcv::gam()] output object
#' @description Function to make a QQ plot for [mgcv::gam()] output object.
#' 
#' @param object - gam object 
#' @param rep - (default = 0)
#' @param level - (default = 0.9) 
#' @param s.rep - (default = 10)
#' @param type - type of residuals ("deviance","pearson","response")
#' @param ... - not used
#' 
#' @return ggplot2 plot object 
#' 
#' @details A ggplot2 version of [mgcv::qq.gam()].
#' 
#' @importFrom wtsPlots getStdTheme
#' @import ggplot2
#' @import mgcv
#'
#' @export 
#' 
qq.gam <- function(object, 
                   rep=0, 
                   level=0.9,
                   s.rep=10,
                   type=c("deviance","pearson","response"),
                   ...) {
  ## get deviance residual quantiles under good fit
  type <- match.arg(type);
  ylab <- paste(type,"residuals");

  if (inherits(object,c("glm","gam"))) {
    if (is.null(object$sig2)) object$sig2 <- summary(object)$dispersion;
  } else stop("object is not a glm or gam")

  ## in case of NA & na.action="na.exclude", we need the "short" residuals:
  object$na.action <- NULL;
  D <- residuals(object,type=type);

  if (object$method %in% c("PQL","lme.ML","lme.REML","lmer.REML","lmer.ML","glmer.ML")) {
    ## then it's come out of a gamm fitter and qq.gam can't see the random effects
    ## that would be necessary to get quantiles. Fall back to normal QQ plot.
    # qqnorm(D,ylab=ylab,pch=pch,...) #--TODO: implement this case
    warning(paste("Object from gam fitter; qq.gam can't see random effects necessary to get quantiles.",
                  "Falling back to normal QQ plot."));
    dfr = data.frame(D=D);
    p = ggplot(dfr,aes(sample=D)) + geom_qq() + 
          geom_abline(slope=1,intercept=0,linetype=2,colour="red") + 
          scale_x_continuous(breaks=seq(-1000,1000,1)) + 
          labs(x="theoretical quantiles",y=ylab) + 
          scale_y_continuous(breaks=seq(-1000,1000,1)) + 
          wtsPlots::getStdTheme();
    return(p);
  }

  lim <- Dq <- NULL;
  if (rep==0) { 
    fam <- mgcv::fix.family.qf(object$family);
    if (is.null(fam$qf))
      rep <- 50; ## try simulation if quantile function not available
    level <- 0;
  } 
  n <- length(D)
  if (rep > 0) { ## simulate quantiles
    fam <- mgcv::fix.family.rd(object$family)
    if (!is.null(fam$rd)) {
      ## simulate deviates... 
      dm <- matrix(0,n,rep)
      for (i in 1:rep) { 
        yr <- fam$rd(object$fitted.values, object$prior.weights, object$sig2)
        object$y <- yr
        dm[,i] <- sort(residuals(object,type=type))
      }
      Dq <- quantile(as.numeric(dm),(1:n - .5)/n) 
    
      ## now get simulation limits on QQ plot
      alpha <- (1-level)/2
      if (alpha>.5||alpha<0) alpha <- .05
      if (level>0&&level<1) lim <- apply(dm,1,FUN=quantile,p=c(alpha,1-alpha)) else
      if (level >= 1) lim <- level 
    }
  } else {
    U <- (1:n-.5)/n
    if (!is.null(fam$qf)) { 
      dm <- matrix(0,n,s.rep)
      for (i in 1:s.rep) { 
        U <- sample(U,n) ## randomize uniform quantiles w.r.t. obs
        q0 <- fam$qf(U,object$fitted.values,object$prior.weights,object$sig2)
        object$y <- q0
        dm[,i] <- sort(residuals(object,type=type)) ## original proposal
      }
      Dq <- sort(rowMeans(dm))
    }
  }
 
  if (!is.null(Dq)) {
    # qqplot(Dq,D,ylab=ylab,xlab="theoretical quantiles",ylim=range(c(lim,D)),
    #        pch=pch)#,...)
    # abline(0,1,col=rl.col)
    # if (!is.null(lim)) {
    #   if (level>=1) for (i in 1:rep) lines(Dq,dm[,i],col=rep.col) else {
    #     n <- length(Dq)
    #     polygon(c(Dq,Dq[n:1],Dq[1]),c(lim[1,],lim[2,n:1],lim[1,1]),col=rep.col,border=NA)
    #   }
    #   abline(0,1,col=rl.col)
    # }
    # points(Dq,sort(D),pch=pch,col=2)#,...)
    # # return(invisible(Dq))
    #--cat("qq type 2\n");
    dfr = as.data.frame(qqplot(Dq,D,ylim=range(c(lim,D)),plot.it=FALSE));
    #--print(head(dfr));
    p = ggplot(dfr,aes(x=x,y=y)) + geom_point() + 
          geom_abline(slope=1,intercept=0,linetype=3,colour="red") + 
          scale_x_continuous(breaks=seq(-1000,1000,1)) + 
          scale_y_continuous(breaks=seq(-1000,1000,1)) + 
          labs(x="theoretical quantiles",y=ylab) + 
          wtsPlots::getStdTheme();
    return(p);
  } else {
    # qqnorm(D,ylab=ylab,pch=pch,...)
    #--cat("qq type 3\n");
    dfr = data.frame(D=D);
    p = ggplot(dfr,aes(sample=D)) + geom_qq() + 
          geom_abline(slope=1,intercept=0,linetype=3,colour="red") + 
          scale_x_continuous(breaks=seq(-1000,1000,1)) + 
          labs(x="theoretical quantiles",y=ylab) + 
          scale_y_continuous(breaks=seq(-1000,1000,1)) + 
          wtsPlots::getStdTheme();
    return(p);
  }
  return(p);
} ## qq.gam
