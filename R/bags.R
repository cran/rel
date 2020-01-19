"bags" <- function(data = NULL, kat = NULL, conf.level = 0.95) {
  
  cl <- match.call()
  data <- as.matrix(na.omit(data))
  nr <- nrow(data)
  nc <- ncol(data)
  data <- matrix(as.numeric(as.factor(data)),nr,nc)
  K <- ifelse(is.numeric(kat),kat,max(data))
  t <- qt(1-(1-conf.level)/2,nr-1)
  
  # Warning
  if (nc != 2) {
    stop("The data frame needs to be formatted as a n*2 matrix!")
  }
  
  # Contingency table
  mat <- ctab(data, K, cl) / nr
  
  # Point estimate/standard error
  po <- sum(mat[row(mat) == col(mat)])
  est <- ((K * po) - 1) / (K - 1)
  se <- sqrt(((K / (K - 1)) ^ 2 * (po * (1 - po))) / (nr - 1))
  ub <- est + (se * t)
  lb <- est - (se * t)
  names(est) <- "Const"
  
  # Export
  y <- structure(list(method="Bennett et al.'s S", call=cl, obs=nc, 
                      sample=nr, est=est, se=se, conf.level=conf.level, 
                      lb=lb, ub=ub, mat=mat, data=data),
                 class = "rel")
  return(y)
  
}
