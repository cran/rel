"spi" <-
  function(data = NULL, weight = c("unweighted","linear","quadratic"), 
           conf.level = 0.95) {
    
    cl <- match.call()
    data <- as.matrix(na.omit(data))
    nr <- nrow(data)
    nc <- ncol(data)
    data <- matrix(as.numeric(as.factor(data)),nr,nc)
    K <- max(data)
    t <- qt(1-(1-conf.level)/2,nr-1)
    
    if (nc==2){
      
      # Contingency table
      mat <- ctab(data, K, cl)
      
      # Weight
      w <- wgts(weight, cl, mat, K)
      method <-
        paste0(ifelse(is.numeric(weight), "custom-weighted", weight), ' pi')

      # Point estimate/standard error
      wmat <- (mat/nr)*w
      po <- sum(wmat)
      pe <- sum(((rowSums(mat)+colSums(mat))/(nr*2))^2*w) 
      se <- sqrt( (1/(1-pe))^2 * (po*(1-po))/(nr-1) )
    
    } else {
      method = paste("Fleiss' kappa")
      mat <- sapply(X=1:K,function(x) rowSums(data==x))
      po <- sum(mat*(mat-1))/((nr*nc)*(nc-1))
      pe <- sum(colSums(mat/(nr*nc))^2)
      
      #se <- (2/(nr*nc*(nc-1))*(pe-((2*nc)-3)*pe^2+2*(nc-2)*sum(colSums(mat/(nr*nc))^3))/(1-pe)^2)^(1/2)
      pj <- colSums(mat)/(nr*nc)
      qj <- 1-pj
      se <- (2/(sum(pj*qj)^2*(nr*nc*(nc-1)))*(sum(pj*qj)^2-sum(pj*qj*(qj-pj))))^(1/2)
    }
    
    est <- (po-pe)/(1-pe)
    names(est) <- "Const"
    ub <- est+(se*t)
    lb <- est-(se*t)

    # Export
    y <- structure(list(method=method, call=cl, obs=nc, sample=nr,
                        est=est, se=se, conf.level=conf.level, 
                        lb=lb, ub=ub, mat=mat, data=data),
                   class = c("rel","spi"))
    return(y)
    
  }