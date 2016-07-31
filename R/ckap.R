"ckap" <-
function(data = NULL, weight = c("unweighted", "linear", "quadratic"), std.err = c("Cohen", "Fleiss"), conf.level = 0.95) {

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
  p.obs <- table(data[,1],data[,2])/nr
  mat[as.numeric(rownames(p.obs)), as.numeric(colnames(p.obs))] <- p.obs
  rs <- rowSums(mat)
  cs <- colSums(mat)
  t <- qt(1-(1-conf.level)/2,nr-1)
  
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
  
  wmat <- mat*w
  wrs <- rowSums(wmat)
  wcs <- colSums(wmat)
  po <- sum(wmat)
  pe <- sum(outer(rs,cs)*w)
  k <- (po-pe)/(1-pe)
  names(k) <- "Const"
  
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
  
  res <- structure(list(method = method,
                        call = cl,
                        sample = nr,
                        est = k,
                        std.err = se,
                        conf.level = conf.level,
                        ci.lower = lb,
                        ci.upper = ub),
                   class="rel")
  return(res)
}