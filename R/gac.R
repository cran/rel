"gac" <-
  function(data = NULL,
           kat = NULL,
           weight = c("unweighted", "linear", "quadratic", "ratio"),
           conf.level = 0.95) {
    cl <- match.call()
    na <- method <- nr <- nc <- K <- t <- zero <- NULL
    list2env(prepd(data, "gac", weight, conf.level, kat), envir = environment())
    
    # Warning
    if (nc != 2) {
      stop("The data frame needs to be formatted as a n*2 matrix!")
    }
    
    # Contingency table
    mat <- ctab(data, K, "gac", zero) / nr
    marg <- (rowSums(mat) + colSums(mat)) / 2
    
    # Weight
    w <- wgts(weight, "gac", mat, K, zero)
    
    # Point estimate
    po <- sum(mat * w)
    pe <- sum(w) * sum(marg * (1 - marg)) / (K * (K - 1))
    est <- (po - pe) / (1 - pe)
    names(est) <- "Const"
    
    # Standard error
    wmat <-
      sum(mat * (w - 2 * (1 - est) * sum(w) * (1 - (
        outer(marg, marg, "+")
      ) / 2) / (K * (K - 1))) ^ 2)
    se <- sqrt((1 / (nr * (1 - pe) ^ 2)) * (wmat - (po - 2 * (1 - est) *
                                                      pe) ^ 2))
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
        mat = mat*nr,
        weight = w,
        data = data
      ),
      class = "rel"
    )
    return(y)
    
  }
