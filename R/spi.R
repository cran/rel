"spi" <-
  function(data = NULL,
           weight = c("unweighted", "linear", "quadratic"),
           conf.level = 0.95) {
    # Prepare
    cl <- match.call()
    na <- method <- nr <- nc <- K <- t <- zero <- NULL
    list2env(prepd(data, "spi", weight, conf.level), envir = environment())
    
    if (nc == 2) {
      # Contingency table
      mat <- ctab(data, K, "spi", zero)
      
      # Weight
      w <- wgts(weight, "spi", mat, K, zero)
      
      # Point estimate/standard error
      wmat <- (mat / nr) * w
      po <- sum(wmat)
      pe <- sum(((rowSums(mat) + colSums(mat)) / (nr * 2)) ^ 2 * w)
      se <- sqrt((1 / (1 - pe)) ^ 2 * (po * (1 - po)) / (nr - 1))
      
    } else {
      method = paste("Fleiss' kappa")
      mat <- sapply(X = 1:K, function(x)
        rowSums(data == x))
      po <- sum(mat * (mat - 1)) / ((nr * nc) * (nc - 1))
      pe <- sum(colSums(mat / (nr * nc)) ^ 2)
      
      #se <- (2/(nr*nc*(nc-1))*(pe-((2*nc)-3)*pe^2+2*(nc-2)*sum(colSums(mat/(nr*nc))^3))/(1-pe)^2)^(1/2)
      pj <- colSums(mat) / (nr * nc)
      qj <- 1 - pj
      se <-
        (2 / (sum(pj * qj) ^ 2 * (nr * nc * (nc - 1))) * (sum(pj * qj) ^ 2 - sum(pj *
                                                                                   qj * (qj - pj)))) ^ (1 / 2)
      w <- NA
    }
    
    est <- (po - pe) / (1 - pe)
    names(est) <- "Const"
    ub <- est + (se * t)
    lb <- est - (se * t)
    
    # Export
    y <- structure(
      list(
        method = method,
        call = cl,
        obs = nc,
        sample = nr,
        est = est,
        se = se,
        conf.level = conf.level,
        lb = lb,
        ub = ub,
        mat = mat,
        weight = w,
        data = data
      ),
      class = c("rel", "spi")
    )
    return(y)
    
  }