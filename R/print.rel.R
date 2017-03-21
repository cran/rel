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
        printCoefmat(cbind("Estimate"=x$est, "StdErr"=x$se,
                           "LowerCB"=x$lb, "UpperCB"=x$ub))
        cat(paste("\n","Confidence level = ",x$conf.level*100,"%","\n",sep=""))
        cat(paste("Sample size = ",x$sample,sep=""))
      },
      ckap = {
        cat("Call:\n"); print(x$call); cat("\n")
        printCoefmat(cbind("Estimate"=x$est, "StdErr"=x$se,
                           "LowerCB"=x$lb, "UpperCB"=x$ub))
        cat(paste("\n","Maximum kappa = ",round(x$kmax,2),sep=""))
        cat(paste("\n","Kappa/maximum kappa = ",round(x$kmax.prop,2),sep=""))
        cat(paste("\n","Confidence level = ",x$conf.level*100,"%","\n",sep=""))
        cat(paste("Observations = ",x$obs,"\n",sep=""))
        cat(paste("Sample size = ",x$sample,sep=""))
      },
      icc = {
        cat("Call:\n"); print(x$call); cat("\n")
        printCoefmat(cbind("Estimate"=x$est, "LowerCB"=x$lb, 
                           "UpperCB"=x$ub))
        cat(paste("\n","Confidence level = ",x$conf.level*100,"%","\n",sep=""))
        cat(paste("Observations = ",x$obs,"\n",sep=""))
        cat(paste("Sample size = ",x$sample,"\n",sep=""))
      },
      kra = {
        cat("Call:\n"); print(x$call); cat("\n")
        printCoefmat(cbind("Estimate"=x$est, "LowerCB"=x$lb, 
                           "UpperCB"=x$ub))
        cat(paste("\n","Confidence level = ",x$conf.level*100,"%","\n",sep=""))
        cat(paste("Observations = ",x$obs,"\n",sep=""))
        cat(paste("Sample size = ",x$sample,"\n",sep=""))
      },
      spi = {
        cat("Call:\n"); print(x$call); cat("\n")
        printCoefmat(cbind("Estimate"=x$est, "StdErr"=x$se,
                           "LowerCB"=x$lb, "UpperCB"=x$ub))
        cat(paste("\n","Confidence level = ",x$conf.level*100,"%","\n",sep=""))
        cat(paste("Observations = ",x$obs,"\n",sep=""))
        cat(paste("Sample size = ",x$sample,sep=""))
      },
      sem = {
        cat("Call:\n"); print(x$call); cat("\n")
        printCoefmat(cbind("Estimate"=x$est, "LowerCB"=x$lb, 
                           "UpperCB"=x$ub))
        cat(paste("\n","Confidence level = ",x$conf.level*100,"%","\n",sep=""))
        cat(paste("Observations = ",x$obs,"\n",sep=""))
        cat(paste("Sample size = ",x$sample,"\n",sep=""))
      }
    )
  }