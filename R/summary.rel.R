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
                         coefficients=cbind("Estimate"=object$est,
                                            "StdErr"=object$std.err,
                                            "LowerCI"=object$ci.lower,
                                            "UpperCI"=object$ci.upper),
                         conf.level=paste(object$conf.level*100,"%",sep=""),
                         sample=paste(object$sample, "subjects"))
             res
           },
           KrA = {
             res <- list(call=object$call,
                         coefficients=cbind("Estimate"=object$est,
                                            "LowerCI"=object$ci.lower,
                                            "UpperCI"=object$ci.upper),
                         conf.level=paste(object$conf.level*100,"%",sep=""),
                         raters=paste(object$raters, "raters"),
                         sample=paste(object$sample, "subjects"),
                         missing=paste(round(object$na,1), "% missing values", sep=""))
             res
             }
           )
    }