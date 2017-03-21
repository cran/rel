"ckap" <-
  function(data = NULL, weight = c("unweighted", "linear", "quadratic"), 
           std.err = c("Fleiss", "Cohen"), conf.level = 0.95, R = 0) {

  cl <- match.call()
  data <- as.matrix(na.omit(data))
  nr <- nrow(data)
  nc <- ncol(data)
  data <- matrix(as.numeric(as.factor(data)),nr,nc)
  mval <- max(data,na.rm=TRUE)
  t <- qt(1-(1-conf.level)/2,nr-1)
  
  if (nc==2){
    #Contingency table
    mat <- matrix(0,mval,mval)
    p.obs <- table(data[,1],data[,2])/nr
    mat[as.numeric(rownames(p.obs)), as.numeric(colnames(p.obs))] <- p.obs
    rs <- rowSums(mat)
    cs <- colSums(mat)
  
    #Weight
    suppressWarnings(
    if (is.numeric(weight)){
      w <- weight
      method = paste("Custom-weighted kappa")
    } else if (weight == "linear"){
      w <- 1-(abs(row(mat)-col(mat))/(mval-1))
      method = paste("Linearly-weighted kappa")
    } else if (weight == "quadratic"){
      w <- 1-(abs(row(mat)-col(mat))/(mval-1))^2
      method = paste("Quadratically-weighted kappa")
    } else{
      w <- diag(ncol(mat))
      method = paste("Cohen's unweighted kappa")
    })
  
    #Point estimate
    wmat <- mat*w
    wrs <- rowSums(wmat)
    wcs <- colSums(wmat)
    po <- sum(wmat)
    pe <- sum(outer(rs,cs)*w)
    k <- (po-pe)/(1-pe)
  
    #Maximum possible point estimate
    pom <- sum(apply(cbind(rs,cs), 1, min, na.rm = TRUE))
    km <- (pom-pe)/(1-pe)
    kmp <- k/km
  
    #Standard error
    if (weight == "unweighted" && std.err == "Cohen"){
      se <- sqrt( ((po*(1-po)) / (1*(1-pe)^2))/nr )
    } else if (weight == "unweighted" && std.err != "Cohen"){
      A <- sum( diag(mat) * (1-((rs+cs)*(1-k)))^2 )
      B <- sum( (1-k)^2 * (mat*outer(cs,rs,"+")^2) ) - sum( diag((1-k)^2*(mat*outer(cs,rs,"+")^2)) )
      C <- (k-pe*(1-k))^2
      se <- sqrt(A+B-C)/((1-pe)*sqrt(nr))
    } else{
      wwrs <- colSums(rs*w)
      wwcs <- colSums(cs*w)
      csrs <- outer(wwcs,wwrs,"+")
      wmat <- sum(mat*(w-csrs*(1-k))^2)
      se <- sqrt(((wmat-(k-pe*(1-k))^2)/nr))/(1-pe)
    }
    ub <- k+(se*t)
    lb <- k-(se*t)
    
  } else {
    method = paste("Conger's kappa")
    ck <- function(data){
      mat <- matrix(unlist(lapply(X=1:mval,function(x) rowSums(data==x))),nr)
      mat2 <- matrix(unlist(lapply(X=1:mval,function(x) colSums(data==x)/nr)),nc,mval)
      po <-  sum(mat*(mat-1))/((nr*nc)*(nc-1))
      pe <- sum((colSums(mat)/(nr*nc))^2)-sum((diag(var(mat2))*(nc-1)/nc)/(nc-1))
      k <- (po-pe)/(1-pe)
    }
    k <- ck(data)
    se <- NA
    km <- NA
    kmp <- NA
    
    #Bootstrap confidence intervals
    if (R == 0 || is.nan(k[[1]])){
      res.boot <- c(NA,NA)
    } else{
      a <- unlist(lapply(X=1:R,function(x) ck(data[sample(nr, replace=TRUE),])))
      res.boot <- quantile(x=a, probs=c((1-conf.level)/2,conf.level+(1-conf.level)/2), na.rm=TRUE) 
    }
    attr(res.boot,"names") <- "Const"
    lb <- res.boot[1]
    ub <- res.boot[2]
  }
  names(k) <- "Const"
  
  res <- structure(list(method = method,
                        call = cl,
                        obs = nc,
                        sample = nr,
                        est = k,
                        se = se,
                        conf.level = conf.level,
                        lb = lb,
                        ub = ub,
                        kmax = km,
                        kmax.prop = kmp,
                        data = data),
                   class = c("rel","ckap"))
  return(res)
}