"sem" <- 
  function(data = NULL, conf.level = 0.95){
    cl <- match.call()
    data <- as.matrix(na.omit(data))
    nr <- nrow(data)
    nc <- ncol(data)
    t <- qt(1-(1-conf.level)/2,nr-1)
    sem <- mean(apply(data,1,sd))
    ub <- sem+t*sd(apply(data,1,sd))/sqrt(nr)
    lb <- sem-t*sd(apply(data,1,sd))/sqrt(nr)
    method = "Standard error of measurement"
    
    names(sem) <- "Const"
    res <- structure(list(method = method, 
                          call = cl, 
                          obs = nc, 
                          sample = nr, 
                          est = sem,
                          conf.level = conf.level, 
                          lb = lb, 
                          ub = ub,
                          data = data), 
                     class = c("rel","sem"))
    return(res)
  }