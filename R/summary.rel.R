"summary.rel" <-
  function(object, ...)
  {
    if (length(class(object)) == 1){ 
      value <- class(object)[1] 
    } else{
      value <- class(object)[2]
    }
    switch(value,
           rel = {
             res <- list(call=object$call,
                         coef=cbind("Estimate"=object$est,
                                    "StdErr"=object$se,
                                    "LowerCB"=object$lb,
                                    "UpperCB"=object$ub),
                         conf.level=paste(object$conf.level*100,"%",sep=""),
                         obs=paste(object$obs, "observations"),
                         sample=paste(object$sample, "subjects"))
             res
           },
           ckap = {
             res <- list(call=object$call,
                         coef=cbind("Estimate"=object$est,
                                    "StdErr"=object$se,
                                    "LowerCB"=object$lb,
                                    "UpperCB"=object$ub),
                         kmax=object$kmax,
                         kmax.prop=object$kmax.prop,
                         conf.level=paste(object$conf.level*100,"%",sep=""),
                         obs=paste(object$obs, "observations"),
                         sample=paste(object$sample, "subjects"))
             res
           },
           icc = {
             res <- list(call=object$call,
                         coef=cbind("Estimate"=object$est,
                                    "LowerCB"=object$lb,
                                    "UpperCB"=object$ub),
                         conf.level=paste(object$conf.level*100,"%",sep=""),
                         obs=paste(object$obs, "observations"),
                         sample=paste(object$sample, "subjects"))
             res
           },
           kra = {
             res <- list(call=object$call,
                         coef=cbind("Estimate"=object$est,
                                    "LowerCB"=object$lb,
                                    "UpperCB"=object$ub),
                         conf.level=paste(object$conf.level*100,"%",sep=""),
                         obs=paste(object$obs, "Observations"),
                         sample=paste(object$sample, "subjects"),
                         missing=paste(round(object$na,1), "% missing values", sep=""))
             res
             },
           sem = {
             res <- list(call=object$call,
                         coef=cbind("Estimate"=object$est,
                                    "LowerCB"=object$lb,
                                    "UpperCB"=object$ub),
                         conf.level=paste(object$conf.level*100,"%",sep=""),
                         obs=paste(object$obs, "observations"),
                         sample=paste(object$sample, "subjects"),
                         missing=paste0(object$na, " missing values"))
             res
           },
           spi = {
             res <- list(call=object$call,
                         coef=cbind("Estimate"=object$est,
                                    "StdErr"=object$se,
                                    "LowerCB"=object$lb,
                                    "UpperCB"=object$ub),
                         kmax=object$kmax,
                         kmax.prop=object$kmax.prop,
                         conf.level=paste(object$conf.level*100,"%",sep=""),
                         obs=paste(object$obs, "observations"),
                         sample=paste(object$sample, "subjects"))
             res
           }
           )
    }