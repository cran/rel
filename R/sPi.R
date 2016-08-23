"sPi" <-
  function(data = NULL, weight = c("unweighted","linear","quadratic"), conf.level = 0.95) {
    
    cl <- match.call()
    data <- data.matrix(data)
    nr <- nrow(data)
    nc <- ncol(data)
    
    if (nc!=2){
      stop("The data frame needs to be formatted as a n*2 matrix!")
    } else if (!is.numeric(data)){
      stop("A numeric data matrix is required")
    } else if (min(data,na.rm=TRUE)<1){
      data <- data+abs(1-min(data,na.rm=TRUE))
    } 
    
    mval <- max(data,na.rm=TRUE)
    mat <- matrix(0,mval,mval)
    obs <- table(data[,1],data[,2])
    mat[as.numeric(rownames(obs)), as.numeric(colnames(obs))] <- obs
    marg <- rowSums(mat)+colSums(mat)
    t <- qt(1-(1-conf.level)/2,nr-1)

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
      method = paste("Scott's unweighted pi")
    })
    
    wmat <- (mat/nr)*w
    po <- sum(wmat)
    pe <- sum((marg/(nr*2))^2*w) 
    spi <- (po-pe)/(1-pe)
    names(spi) <- "Const"

    se <- sqrt( (1/(1-pe))^2 * (po*(1-po))/(nr-1) )
    ub <- spi+(se*t)
    lb <- spi-(se*t)
    
    res <- structure(list(method = method,
                          call = cl,
                          sample = nr,
                          est = spi,
                          std.err = se,
                          conf.level = conf.level,
                          ci.lower = lb,
                          ci.upper = ub),
                     class="rel")
    return(res)
  }