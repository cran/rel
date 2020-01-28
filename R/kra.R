"kra" <- 
  function(data = NULL, weight = c("nominal","ordinal","interval","ratio"), 
           conf.level = 0.95, R = 0) {

    cl <- match.call()
    data <- as.matrix(data)
    nr <- nrow(data)
    nc <- ncol(data)
    data <- matrix(as.numeric(as.factor(data)),nr,nc)
    na <- sum(is.na(data))/(nr*nc)*100
    method <-
      paste0("Krippendorf's alpha with ",
             ifelse(is.numeric(weight), "custom", weight),
             " weight")
    
    # Contingency table
    K <- max(data,na.rm=TRUE)
    mat <- ctab(data,K,"kra")
    
    alfa <- function(data,weight,cl,K){
      
      # Contingency table
      mat <- ctab(data, K, cl)
      marg <- rowSums(mat)
      
      # Weight
      w <- wgts(weight, cl, mat, K)
      
      # Point estimate
      if (nrow(mat)==1 && ncol(mat)==1){
        vs <- matrix(c(1,1),2,1)
      } else {
        vs <- combn(1:K,2)
      }
      est <- 1-(sum(marg)-1)*sum(mat[upper.tri(mat, diag = FALSE)]*w[upper.tri(w,diag=FALSE)])/
        sum(marg[vs[1,]]*marg[vs[2,]]*t(w[lower.tri(w,diag=FALSE)]))
      
      return(est)
    }
    est <- alfa(data,weight,"kra",K)
    
    # Bootstrapped confidence intervals
    if (R == 0 || is.nan(est)){
      cb <- c(NA,NA)
    } else{
      cb <- quantile(sapply(1:R, function(x) alfa(data[sample(nr, replace=TRUE),], weight,cl,K)),
                     probs=c((1-conf.level)/2,conf.level+(1-conf.level)/2), na.rm=TRUE)
    }
    attr(cb,"names") <- "Const"

    # Export
    y <- structure(list(method=method, call=cl, obs=nc, sample=nr,
                        est=est, se=NA, conf.level=conf.level,
                        lb=cb[1], ub=cb[2], mat=mat, data=data,
                        na=na),
                   class = c("rel","kra"))
    return(y)
    
  }
