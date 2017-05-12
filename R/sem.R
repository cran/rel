"sem" <- 
  function(data = NULL, type = c("mse", "sd", "cpd"), conf.level = 0.95){
    cl <- match.call()
    
    if (type=="mse"||type=="sd"||missing(type)){
      data <- as.matrix(na.omit(data))
      nr <- nrow(data)
      nc <- ncol(data)
      semT <- NA
      sem <- ifelse(type=="mse"||missing(type),
             ((sum((data-rowMeans(data))^2)-sum(nr*(colMeans(data)-mean(data))^2))/((nr-1)*(nc-1)))^(1/2),
             mean(apply(data,1,var))^(1/2))
      t <- qt(1-(1-conf.level)/2,nr-1)
      ub <- sem+t*sd(apply(data,1,sd,na.rm=TRUE))/sqrt(nr)
      lb <- sem-t*sd(apply(data,1,sd,na.rm=TRUE))/sqrt(nr)
    } else if (type=="cpd"){
      data <- as.matrix(data[rowSums(!is.na(data)) > 0,colSums(!is.na(data)) > 0])
      nr <- nrow(data)
      nc <- ncol(data)
      semT <- apply(diff(t(data)),1,sd,na.rm=TRUE)/sqrt(2) 
      sem <- drop(semT^2 %*% (rowSums(!is.na(diff(t(data))))-1)/sum(rowSums(!is.na(diff(t(data))))-1))^(1/2)
      DF <- (1-0.22*sum(!is.na(data))/(nr*nc))*sum(rowSums(!is.na(diff(t(data))))-1)
      ub <- sqrt(DF*sem^2/qchisq(1-(1-0.95)/2,DF,lower.tail=FALSE))
      lb <- sqrt(DF*sem^2/qchisq(1-(1-0.95)/2,DF,lower.tail=TRUE))
    }
    na <- sum(is.na(data))
    method = "Standard error of measurement"
    
    names(sem) <- "Const"
    res <- structure(list(method = method,
                          call = cl,
                          obs = nc,
                          sample = nr,
                          na = na,
                          est = sem,
                          conf.level = conf.level,
                          lb = lb,
                          ub = ub,
                          est.cpd = semT,
                          data = data), 
                     class = c("rel","sem"))
    return(res)
  }




