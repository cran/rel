"bags" <- function(data = NULL, kat = NULL, conf.level = 0.95) {
  
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
  
  if (is.numeric(kat)){
    mval <- kat
  } else{
    mval <- max(data,na.rm=TRUE)
  }
  
  mat <- matrix(0,mval,mval)
  p.obs <- table(data[,1],data[,2])/nr
  mat[as.numeric(rownames(p.obs)), as.numeric(colnames(p.obs))] <- p.obs
  t <- qt(1-(1-conf.level)/2,nr-1)
  po <- sum(mat[row(mat) == col(mat)])
  S <- ((mval*po)-1)/(mval-1) 
  names(S) <- "Const"
  se <- sqrt( ((mval/(mval-1))^2 * (po*(1-po)))/(nr-1) )
  ub <- S+(se*t)
  lb <- S-(se*t)
  
  res <- structure(list(method = "Bennett et al.'s S",
                        call = cl,
                        sample = nr,
                        est = S,
                        std.err = se,
                        conf.level = conf.level,
                        ci.lower = lb,
                        ci.upper = ub),
                      class="rel")
  return(res)
}