"icc" <- 
  function(data = NULL, model = c("one", "two"), type = c("agreement", "consistency"), 
           measure = c("single", "average"), conf.level = 0.95){
    cl <- match.call()
    data <- as.matrix(na.omit(data))
    nr <- nrow(data)
    nc <- ncol(data)
    
    #Degrees of freedom
    dfr <- nr-1 #between rows
    dfw <- nr*(nc-1) #within rows
    dfc <- nc-1 #between columns
    dfe <- (nr-1)*(nc-1) #error
    
    #Mean squares
    MSr <- sum(nc*(rowMeans(data)-mean(data))^2)/dfr
    MSw <- sum((data-rowMeans(data))^2)/dfw
    MSc <- sum(nr*(colMeans(data)-mean(data))^2)/dfc
    MSe <- (sum((data-rowMeans(data))^2)-sum(nr*(colMeans(data)-mean(data))^2))/dfe
    
    #ICC models
    if (model == "one" && measure == "single"){
      icc <- (MSr-MSw)/(MSr+(nc-1)*MSw)
      Fu <- (MSr/MSw)*qf(1-(1-conf.level)/2,dfw,dfr)
      Fl <- (MSr/MSw)/qf(1-(1-conf.level)/2,dfr,dfw) 
      ub <- (Fu-1)/(Fu+dfc)
      lb <- (Fl-1)/(Fl+dfc)
      method = paste("ICC1(A,1); one-way random effects model of single-measure absolute agreement")
    } else if (model == "one" && measure == "average"){
      icc <- (MSr-MSw)/MSr
      Fu <- (MSr/MSw)*qf(1-(1-conf.level)/2,dfw,dfr)
      Fl <- (MSr/MSw)/qf(1-(1-conf.level)/2,dfr,dfw) 
      ub <- 1-(1/Fu)
      lb <- 1-(1/Fl)
      method = paste("ICC1(A,",nc,"); ",sep="","one-way random effects model of average-measure absolute agreement")
    } else if (model == "two" && type == "agreement" && measure == "single"){
      icc <- (MSr-MSe)/(MSr+(nc-1)*MSe+(nc/nr)*(MSc-MSe))
      a <- (nc*icc)/(nr*(1-icc))
      b <- 1+(nc*icc*dfr)/(nr*(1-icc))
      v <- (a*MSc+b*MSe)^2/((a*MSc)^2/dfc+(b*MSe)^2/(dfr*dfc))
      Fu <- qf(1-(1-conf.level)/2,v,dfr)
      Fl <- qf(1-(1-conf.level)/2,dfr,v)
      ub <- (nr*(Fu*MSr-MSe))/(nc*MSc+(nc*nr-nc-nr)*MSe+nr*Fu*MSr)
      lb <- (nr*(MSr-Fl*MSe))/(Fl*(nc*MSc+(nc*nr-nc-nr)*MSe)+nr*MSr)
      method = paste("ICC2(A,1); two-way random effects model of single-measure absolute agreement")
    } else if (model == "two" && type == "agreement" && measure == "average"){
      icc <- (MSr-MSe)/(MSr+(MSc-MSe)/nr)
      a <- (nc*icc)/(nr*(1-icc))
      b <- 1+(nc*icc*dfr)/(nr*(1-icc))
      v <- (a*MSc+b*MSe)^2/((a*MSc)^2/dfc+(b*MSe)^2/(dfr*dfc))
      Fu <- qf(1-(1-conf.level)/2,v,dfr)
      Fl <- qf(1-(1-conf.level)/2,dfr,v)
      ub <- (nr*(Fu*MSr-MSe))/(MSc-MSe+nr*Fu*MSr)
      lb <- (nr*(MSr-Fl*MSe))/(Fl*(MSc-MSe)+nr*MSr)
      method = paste("ICC2(A,",nc,"); ",sep="","two-way random effects model of average-measure absolute agreement")
    } else if (model == "two" && type == "consistency" && measure == "single"){
      icc <- (MSr-MSe)/(MSr+(nc-1)*MSe)
      Fu <- (MSr/MSe)*qf(1-(1-conf.level)/2,dfe,dfr)
      Fl <- (MSr/MSe)/qf(1-(1-conf.level)/2,dfr,dfe)   
      ub <- (Fu-1)/(Fu+dfc)
      lb <- (Fl-1)/(Fl+dfc)
      method = paste("ICC2(C,1); two-way random effects model of single-measure consistency")
    } else if (model == "two" && type == "consistency" && measure == "average"){
      icc <- (MSr-MSe)/MSr
      Fu <- (MSr/MSe)*qf(1-(1-conf.level)/2,dfe,dfr)
      Fl <- (MSr/MSe)/qf(1-(1-conf.level)/2,dfr,dfe)   
      ub <- 1-(1/Fu)
      lb <- 1-(1/Fl)
      method = paste("ICC2(C,",nc,"); ",sep="","two-way random effects model of average-measure consistency")
    }
    
    names(icc) <- "Const"
    res <- structure(list(method = method, 
                          call = cl, 
                          obs = nc, 
                          sample = nr, 
                          est = icc,
                          conf.level = conf.level, 
                          lb = lb, 
                          ub = ub,
                          data = data), 
                     class = c("rel","icc"))
    return(res)
  }