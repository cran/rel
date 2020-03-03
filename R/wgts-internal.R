"wgts" <-
  function(weight, cl, mat, K, zero) {
    # Fix category zero problem
    if (zero == TRUE) {
      R <- row(mat) - 1
      C <- col(mat) - 1
    } else{
      R <- row(mat)
      C <- col(mat)
    }
    
    # Compute weights
    if (is.numeric(weight)) {
      w <- weight
    } else if (any(grepl("^quadratic$", weight))) {
      w <- 1 - (abs(R - C) / (K - 1)) ^ 2
    } else if (any(grepl("^linear$", weight))) {
      w <- 1 - (abs(R - C) / (K - 1))
    } else  if (any(grepl("^unweighted$", weight))) {
      w <- diag(K)
    } else  if (any(grepl("^ratio$", weight)) &&
                any(grepl("kra", cl))) {
      w <- ((R - C) / (R + C)) ^ 2
    } else if (any(grepl("^ratio$", weight)) &&
               any(grepl("gac", cl))) {
      w <-
        1 - ((R - C) / (R + C)) ^ 2 / ((K - 1) / (K +
                                                    1)) ^ 2
    } else  if (any(grepl("^interval$", weight))) {
      w <- (R - C) ^ 2
    } else  if (any(grepl("^ordinal$", weight))) {
      w <- matrix(0, K, K)
      for (i in 2:(K - 1)) {
        w[i - 1, (i + 1):K] <- t(cumsum(rowSums(mat)[i:(K - 1)]))
      }
      w[upper.tri(w)] <-
        w[upper.tri(w)] + (outer(rowSums(mat), rowSums(mat), "+") / 2)[upper.tri(w)]
      w <- (w + t(w)) ^ 2
    } else  if (any(grepl("^nominal$", weight))) {
      w <- abs(diag(ncol(mat)) - 1)
    } else {
      stop("Please provide a valid weight")
    }
    
    return(w)
    
  }