"ckap" <-
  function(data = NULL, weight = c("unweighted", "linear", "quadratic"), 
           std.err = c("Fleiss", "Cohen"), conf.level = 0.95, R = 0) {

  # Prepare
  cl <- match.call()
  data <- as.matrix(na.omit(data))
  nr <- nrow(data)
  nc <- ncol(data)
  data <- matrix(as.numeric(as.factor(data)), nr, nc)
  K <- max(data)
  t <- qt(1 - (1 - conf.level) / 2, nr - 1)
 
  if (nc==2){
    
    # Contingency table
    mat <- ctab(data, K, "ckap") / nr
    rs <- rowSums(mat)
    cs <- colSums(mat)
    
    # Weight
    w <- wgts(weight, "ckap", mat, K)
    method <-
      paste0(ifelse(is.numeric(weight), "custom-weighted", weight), ' kappa')
    
    # Point estimate
    po <- sum(mat*w)
    pe <- sum((rs%o%cs)*w)
    est <- (po-pe)/(1-pe)
  
    # Maximum kappa
    pom <- sum(pmin(rs,cs))
    pem <- (K>2)*(ifelse(1/K>=pom/(K-2), 
                         pom^2/(K-2), 
                         1/K^2*(K-2)+1/(2*K^2)*(K*pom-K+2)*(K-K*pom+2)*(all(weight=="quadratic")||is.numeric(weight)))
                  + 1/K*(all(weight=="linear"))
                  + pe*(sum(w)==K)) + pe*(K<=2)
    kmax <- (pom-pem)/(1-pem)
    kmax.prop <- est/kmax
  
    # Standard error
    
    
    if ( any(grepl("^unweighted$", weight)) && any(grepl("^Cohen$", std.err)) ){
      se <- sqrt( ((po*(1-po)) / (1*(1-pe)^2))/nr )
    } else if ( any(grepl("^unweighted$", weight)) && any(grepl("^Fleiss$", std.err)) ){
      A <- sum( diag(mat) * (1-((rs+cs)*(1-est)))^2 )
      B <- sum( (1-est)^2 * (mat*outer(cs,rs,"+")^2) ) - sum( diag((1-est)^2*(mat*outer(cs,rs,"+")^2)) )
      C <- (est-pe*(1-est))^2
      se <- sqrt(A+B-C)/((1-pe)*sqrt(nr))
    } else{
      csrs <- outer(c(cs%*%w),c(rs%*%w),"+")
      wmat <- sum(mat*(w-csrs*(1-est))^2)
      se <- sqrt(((wmat-(est-pe*(1-est))^2)/nr))/(1-pe)
    }
    ub <- est+(se*t)
    lb <- est-(se*t)
    
  } else {
    method = paste("Conger's kappa")
    ck <- function(data){
      mat <- sapply(X=1:K,function(x) rowSums(data==x))
      mat2 <- sapply(X=1:K,function(x) colSums(data==x)/nr)
      po <-  sum(mat*(mat-1))/((nr*nc)*(nc-1))
      pe <- sum((colSums(mat)/(nr*nc))^2)-sum((diag(var(mat2))*(nc-1)/nc)/(nc-1))
      k <- (po-pe)/(1-pe)
    }
    est <- ck(data)
    se <- kmax <- kmax.prop <- mat <- NA
    
    # Bootstraped confidence intervals
    if (R == 0 || is.nan(est)){
      cb <- c(NA,NA)
    } else{
      cb <- quantile(sapply(X=1:R,function(x) ck(data[sample(nr, replace=TRUE),])),
                     probs=c((1-conf.level)/2,conf.level+(1-conf.level)/2), na.rm=TRUE) 
    }
    attr(cb,"names") <- "Const"
    lb <- cb[1]
    ub <- cb[2]
  }
  names(est) <- "Const"
  
  # Export
  y <- structure(list(method=method, call=cl, obs=nc, sample=nr,
                      est=est, se=se, conf.level=conf.level, 
                      lb=lb, ub=ub, mat=mat, data=data,
                      kmax=kmax, kmax.prop=kmax.prop),
                 class = c("rel","ckap"))
  return(y)
  
  }