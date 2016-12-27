"krA" <- 
  function(data = NULL, metric = c("nominal","ordinal","interval","ratio"), conf.level = 0.95, R = 0) {

    cl <- match.call()
    data <- as.matrix(data)
    nr <- nrow(data)
    nc <- ncol(data)
    data <- matrix(as.numeric(as.factor(data)),nr,nc)
    na <- sum(is.na(data))/(nr*nc)*100
    
    A <- function(data,metric){
    
      #Contingency table
      mval <- max(data,na.rm=TRUE)
      mu <- rowSums(!is.na(data))
      ap <- expand.grid(seq_len(nc),seq_len(nc))
      ap <- ap[ap[,1]!=ap[,2],]
    
      corr <- function(x){
        tab <- quote(na.omit(table(data[x,ap[,1]],data[x,ap[,2]])/(mu[x]-1)))
        mat <- matrix(0,mval,mval) 
        mat[as.numeric(rownames(eval(tab))),as.numeric(colnames(eval(tab)))] <- eval(tab) 
        return(mat)
      }
      mat <- Reduce("+",lapply(X=1:nr,FUN=corr))
      marg <- rowSums(mat)
      
      #Metrics
      suppressWarnings(
      if (metric == "ratio"){  
        metric <- ( (row(mat)-col(mat))/(row(mat)+col(mat)) )^2
        method = paste("Krippendorf's alpha for ratio data")
      } else if (metric == "interval"){
        metric <- (row(mat)-col(mat))^2
        method = paste("Krippendorf's alpha for interval data")
      } else if (metric == "ordinal"){
        ends <- outer(marg,marg,"+")/2
        ord <- function(x){
          res <- matrix(0,mval,mval)
          if (x<=mval-1){
            res[x-1,(x+1):(mval)] <- t(matrix(cumsum(marg[(x):(mval-1)])))
          }  else{
            res <- ends
          }
          return(res)
        }
        cs <- Reduce("+",lapply(X=2:(mval-1),FUN=ord))
        metric <- (ifelse(col(cs)>row(cs)+1,cs+ends,ends))^2
        metric[lower.tri(metric,diag=TRUE)] <- 0
        metric <- metric+t(metric)
        method = paste("Krippendorf's alpha for ordinal data")
      } else{
        metric <- abs(diag(ncol(mat))-1)
        method = paste("Krippendorf's alpha for nominal data")
      })
    
      #Point estimate
      if (dim(mat)[1]==1 && dim(mat)[2]==1){
        vs <- matrix(c(1,1),2,1)
      } else {
        vs <- combn(1:mval,2) 
      }
      ka <- 1-(sum(marg)-1)*sum(mat[upper.tri(mat, diag = FALSE)]*metric[upper.tri(metric,diag=FALSE)])/
        sum(marg[vs[1,]]*marg[vs[2,]]*t(metric[lower.tri(metric,diag=FALSE)]))
      return(list(ka,method))
    }
    out <- A(data=data,metric=metric)
    
    #Bootstrapped confidence intervals
    if (R == 0 || is.nan(out[[1]])){
      res.boot <- c(NA,NA)
    } else{
      a <- unlist(lapply(X=1:R,function(x) A(data[sample(nr, replace=TRUE),], metric=metric)[[1]]))
      res.boot <- quantile(x=a, probs=c((1-conf.level)/2,conf.level+(1-conf.level)/2), na.rm=TRUE) 
    }
    out[is.nan(out[[1]])] <- 0
    attr(res.boot,"names") <- "Const"
    
    res <- structure(list(method = out[[2]],
                          call = cl,
                          raters = nc,
                          sample = nr,
                          na = na,
                          est = out[[1]],
                          conf.level = conf.level,
                          ci.lower = res.boot[1],
                          ci.upper = res.boot[2]),
                     class=c("rel","krA"))
    return(res)
  }