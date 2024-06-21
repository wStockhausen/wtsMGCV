plotRandomEffect <- function(x,P=NULL,data=NULL,label="",se1.mult=1,se2.mult=2,
                     partial.resids=FALSE,rug=TRUE,se=TRUE,scale=-1,n=100,n2=40,n3=3,
                     theta=30,phi=30,jit=FALSE,xlab=NULL,ylab=NULL,main=NULL,
                     ylim=NULL,xlim=NULL,too.far=0.1,shade=FALSE,shade.col="gray80",
                     shift=0,trans=I,by.resids=FALSE,scheme=0,...) {
## plot method for a "random.effect" smooth class
 
  if (is.null(P)) { ## get plotting information...
    if (!x$plot.me) return(NULL) else { ## shouldn't or can't plot 
      raw <- data[x$term][[1]]
      p <- x$last.para - x$first.para + 1
      X <- diag(p)   # prediction matrix for this term
      if (is.null(xlab)) xlabel<- "Gaussian quantiles" else xlabel <- xlab
      if (is.null(ylab)) ylabel <- "effects" else ylabel <- ylab
      if (!is.null(main)) label <- main
      return(list(X=X,scale=FALSE,se=FALSE,raw=raw,xlab=xlabel,ylab=ylabel,
             main=label))

    } ## end of basic plot data production 
  } else { ## produce plot
    b <- as.numeric(trans(P$fit+shift))
    qqnorm(b,main=P$main,xlab=P$xlab,ylab=P$ylab,...)
    qqline(b)
    dfr = data.frame(D=D);
    p = ggplot(dfr,aes(sample=D)) + geom_qq() + 
          geom_abline(slope=1,intercept=0,linetype=3,colour="red") + 
          scale_x_continuous(breaks=seq(-1000,1000,1)) + 
          labs(x="theoretical quantiles",y=ylab) + 
          scale_y_continuous(breaks=seq(-1000,1000,1)) + 
          wtsPlots::getStdTheme();
    return(p);
  } ## end of plot production
} ## end of plot.random.effect
