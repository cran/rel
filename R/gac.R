"gac" <-
  function(data = NULL, kat = NULL, weight = c("unweighted", "linear", "quadratic", "ratio"), 
           conf.level = 0.95) {
    
    cl <- match.call()
    data <- as.matrix(na.omit(data))
    nr <- nrow(data)
    nc <- ncol(data)
    data <- matrix(as.numeric(as.factor(data)),nr,nc)
    K <- ifelse(is.numeric(kat),kat,max(data))
    t <- qt(1-(1-conf.level)/2,nr-1)
    method <-
      paste0(
        ifelse(is.numeric(weight), "custom-weighted", weight),
        ifelse(grep("^unweighted$",weight), " AC1", " AC2")
      )
    
    # Warning
    if (nc != 2) {
      stop("The data frame needs to be formatted as a n*2 matrix!")
    }
    
    # Contingency table
    mat <- ctab(data, K, "gac") / nr
    marg <- (rowSums(mat) + colSums(mat)) / 2
    
    # Weight
    w <- wgts(weight, "gac", mat, K)
    
    # Point estimate
    po <- sum(mat * w)
    pe <- sum(w) * sum(marg * (1 - marg)) / (K * (K - 1))
    est <- (po - pe) / (1 - pe)
    names(est) <- "Const"
    
    # Standard error
    wmat <- sum(mat * (w-2*(1-est)*sum(w)*(1-(outer(marg,marg,"+"))/2)/(K*(K-1)))^2) 
    se <- sqrt( (1/(nr*(1-pe)^2))*(wmat-(po-2*(1-est)*pe)^2) )
    ub <- est+(se*t)
    lb <- est-(se*t)

    # Export
    y <- structure(list(method=method, call=cl, obs=nc, sample=nr,
                        est=est, se=se, conf.level=conf.level, 
                        lb=lb, ub=ub, mat=mat, data=data),
                   class = "rel")
    return(y)
    
  }