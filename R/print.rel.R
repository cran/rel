"print.rel" <-
  function(x, ...)
  { 
    if (length(class(x)) == 1){ 
      value <- class(x)[1] 
      } else{
        value <- class(x)[2]
      }
    switch(value,
      rel = {
        cat("Call:\n"); print(x$call); cat("\n")
        printCoefmat(cbind("Estimate"=x$est, "StdErr"=x$std.err,
                           "LowerCI"=x$ci.lower, "UpperCI"=x$ci.upper))
        cat(paste("\n","Confidence level = ",x$conf.level*100,"%","\n",sep=""))
        cat(paste("Sample size = ",x$sample,sep=""))
      },
      cKap = {
        cat("Call:\n"); print(x$call); cat("\n")
        printCoefmat(cbind("Estimate"=x$est, "StdErr"=x$std.err,
                           "LowerCI"=x$ci.lower, "UpperCI"=x$ci.upper))
        cat(paste("\n","Maximum kappa = ",round(x$kmax,2),sep=""))
        cat(paste("\n","Kappa/maximum kappa = ",round(x$kmax.prop,2),sep=""))
        cat(paste("\n","Confidence level = ",x$conf.level*100,"%","\n",sep=""))
        cat(paste("Raters = ",x$raters,"\n",sep=""))
        cat(paste("Sample size = ",x$sample,sep=""))
      },
      krA = {
        cat("Call:\n"); print(x$call); cat("\n")
        printCoefmat(cbind("Estimate"=x$est, "LowerCI"=x$ci.lower, 
                           "UpperCI"=x$ci.upper))
        cat(paste("\n","Confidence level = ",x$conf.level*100,"%","\n",sep=""))
        cat(paste("Raters = ",x$raters,"\n",sep=""))
        cat(paste("Sample size = ",x$sample,"\n",sep=""))
      },
      sPi = {
        cat("Call:\n"); print(x$call); cat("\n")
        printCoefmat(cbind("Estimate"=x$est, "StdErr"=x$std.err,
                           "LowerCI"=x$ci.lower, "UpperCI"=x$ci.upper))
        cat(paste("\n","Confidence level = ",x$conf.level*100,"%","\n",sep=""))
        cat(paste("Raters = ",x$raters,"\n",sep=""))
        cat(paste("Sample size = ",x$sample,sep=""))
      }
    )
  }