"sPi" <-
  function(data = NULL, weight = c("unweighted","linear","quadratic"), conf.level = 0.95) {
    
    cl <- match.call()
    data <- as.matrix(na.omit(data))
    nr <- nrow(data)
    nc <- ncol(data)
    data <- matrix(as.numeric(as.factor(data)),nr,nc)
    mval <- max(data,na.rm=TRUE)
    t <- qt(1-(1-conf.level)/2,nr-1)
    
    if (nc==2){
      mat <- matrix(0,mval,mval)
      obs <- table(data[,1],data[,2])
      mat[as.numeric(rownames(obs)), as.numeric(colnames(obs))] <- obs
      marg <- rowSums(mat)+colSums(mat)

      suppressWarnings(
      if (is.numeric(weight)){
        w <- weight
        method = paste("Custom-weighted pi")
      } else if (weight == "linear"){
        w <- 1-(abs(row(mat)-col(mat))/(mval-1))
        method = paste("Linearly-weighted pi")
      } else if (weight == "quadratic"){
        w <- 1-(abs(row(mat)-col(mat))/(mval-1))^2
        method = paste("Quadratically-weighted pi")
      } else{
        w <- diag(ncol(mat))
        method = paste("Scott's pi")
      })
    
      wmat <- (mat/nr)*w
      po <- sum(wmat)
      pe <- sum((marg/(nr*2))^2*w) 
      se <- sqrt( (1/(1-pe))^2 * (po*(1-po))/(nr-1) )
    
    } else {
      method = paste("Fleiss' kappa")
      mat <- matrix(unlist(lapply(X=1:mval,function(x) rowSums(data==x))),nr)
      po <- sum(mat*(mat-1))/((nr*nc)*(nc-1))
      pe <- sum(colSums(mat/(nr*nc))^2)
      
      #se <- (2/(nr*nc*(nc-1))*(pe-((2*nc)-3)*pe^2+2*(nc-2)*sum(colSums(mat/(nr*nc))^3))/(1-pe)^2)^(1/2)
      pj <- colSums(mat)/(nr*nc)
      qj <- 1-pj
      se <- (2/(sum(pj*qj)^2*(nr*nc*(nc-1)))*(sum(pj*qj)^2-sum(pj*qj*(qj-pj))))^(1/2)
    }
    
    spi <- (po-pe)/(1-pe)
    names(spi) <- "Const"
    ub <- spi+(se*t)
    lb <- spi-(se*t)
    
    res <- structure(list(method = method,
                          call = cl,
                          raters = nc,
                          sample = nr,
                          est = spi,
                          std.err = se,
                          conf.level = conf.level,
                          ci.lower = lb,
                          ci.upper = ub),
                     class=c("rel","sPi"))
    return(res)
  }