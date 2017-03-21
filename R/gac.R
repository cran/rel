"gac" <-
  function(data = NULL, kat = NULL, weight = c("unweighted","linear","quadratic","ratio"), 
           conf.level = 0.95) {
    
    cl <- match.call()
    data <- as.matrix(na.omit(data))
    nr <- nrow(data)
    nc <- ncol(data)
    data <- matrix(as.numeric(as.factor(data)),nr,nc)
    
    if (nc!=2){
      stop("The data frame needs to be formatted as a n*2 matrix!")
    } 
    
    if (is.numeric(kat)){
      mval <- kat
    } else{
      mval <- max(data,na.rm=TRUE)
    }
    
    mat <- matrix(0,mval,mval)
    p.obs <- table(data[,1],data[,2])/nr
    mat[as.numeric(rownames(p.obs)), as.numeric(colnames(p.obs))] <- p.obs
    marg <- (rowSums(mat)+colSums(mat))/2
    t <- qt(1-(1-conf.level)/2,nr-1)
    
    suppressWarnings(
    if (is.numeric(weight)) {
      w <- weight
      method = paste("Gwet's custom-weighted AC2")
    } else if (weight == "linear"){
      w <- 1-(abs(row(mat)-col(mat))/(mval-1))
      method = paste("Gwet's linearly-weighted AC2")
    } else if (weight == "quadratic"){
      w <- 1-(abs(row(mat)-col(mat))/(mval-1))^2
      method = paste("Gwet's quadratically-weighted AC2")
    } else if (weight == "ratio"){
      w <- 1-((row(mat)-col(mat))/(row(mat)+col(mat)))^2/((mval-1)/(mval+1))^2
      method = paste("Gwet's ratio-weighted AC2")
    } else {
      w <- diag(mval)
      method = paste("Gwet's AC1")
    })

    po <- sum(mat*w)
    pe <- sum(w) * sum(marg*(1-marg))/(mval*(mval-1))
    g <- (po-pe)/(1-pe)
    names(g) <- "Const"
    
    wmat <- sum(mat * (w-2*(1-g)*sum(w)*(1-(outer(marg,marg,"+"))/2)/(mval*(mval-1)))^2) 
    se <- sqrt( (1/(nr*(1-pe)^2))*(wmat-(po-2*(1-g)*pe)^2) )
    ub <- g+(se*t)
    lb <- g-(se*t)
    
    res <- structure(list(method = method,
                          call = cl,
                          obs = nc,
                          sample = nr,
                          est = g,
                          se = se,
                          conf.level = conf.level,
                          lb = lb,
                          ub = ub,
                          data = data),
                     class = "rel")
    return(res)
  }