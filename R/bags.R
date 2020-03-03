"bags" <- function(data = NULL,
                   kat = NULL,
                   conf.level = 0.95) {
  # Prepare
  cl <- match.call()
  na <- method <- weight <- nr <- nc <- K <- t <- zero <- NULL
  list2env(prepd(data, "bags", weight, conf.level, kat), envir = environment())
  
  # Warning
  if (nc != 2) {
    stop("The data frame needs to be formatted as a n*2 matrix!")
  }
  
  # Contingency table
  mat <- ctab(data, K, "bags", zero) / nr
  
  # Point estimate/standard error
  po <- sum(mat[row(mat) == col(mat)])
  est <- ((K * po) - 1) / (K - 1)
  se <- sqrt(((K / (K - 1)) ^ 2 * (po * (1 - po))) / (nr - 1))
  ub <- est + (se * t)
  lb <- est - (se * t)
  names(est) <- "Const"
  
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
      weight = NA,
      data = data
    ),
    class = "rel"
  )
  return(y)
  
}
