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
             res <- with(object, 
                         list(call=call,
                              coef=cbind("Estimate"=est,
                                    "StdErr"=se,
                                    "LowerCB"=lb,
                                    "UpperCB"=ub),
                              conf.level=paste(conf.level*100,"%",sep=""),
                              obs=paste(obs, "observations"),
                              sample=paste(sample, "subjects")) )
             res
           },
           ckap = {
             res <- with(object, 
                         list(call=call,
                              coef=cbind("Estimate"=est,
                                         "StdErr"=se,
                                         "LowerCB"=lb,
                                         "UpperCB"=ub),
                              kmax=kmax,
                              kmax.prop=kmax.prop,
                              conf.level=paste(conf.level*100,"%",sep=""),
                              obs=paste(obs, "observations"),
                              sample=paste(sample, "subjects")) )
             res
           },
           icc = {
             res <- with(object,
                         list(call=call,
                              coef=cbind("Estimate"=est,
                                         "LowerCB"=lb,
                                         "UpperCB"=ub),
                              conf.level=paste(conf.level*100,"%",sep=""),
                              obs=paste(obs, "observations"),
                              sample=paste(sample, "subjects")) )
             res
           },
           kra = {
             res <- with(object,
                         list(call=call,
                              coef=cbind("Estimate"=est,
                                         "LowerCB"=lb,
                                         "UpperCB"=ub),
                              conf.level=paste(conf.level*100,"%",sep=""),
                              obs=paste(obs, "Observations"),
                              sample=paste(sample, "subjects"),
                              missing=paste(round(na,1), "% missing values", sep="")) )
             res
             },
           sem = {
             res <- with(object, 
                         list(call=call,
                              coef=cbind("Estimate"=est,
                                         "LowerCB"=lb,
                                         "UpperCB"=ub),
                              conf.level=paste(conf.level*100,"%",sep=""),
                              obs=paste(obs, "observations"),
                              sample=paste(sample, "subjects"),
                              missing=paste0(na, " missing values")) )
             res
           },
           spi = {
             res <- with(object,
                         list(call=call,
                              coef=cbind("Estimate"=est,
                                         "StdErr"=se,
                                         "LowerCB"=lb,
                                         "UpperCB"=ub),
                              kmax=kmax,
                              kmax.prop=kmax.prop,
                              conf.level=paste(conf.level*100,"%",sep=""),
                              obs=paste(obs, "observations"),
                              sample=paste(sample, "subjects")) )
             res
           }
           )
  }

