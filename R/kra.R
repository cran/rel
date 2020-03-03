"kra" <-
  function(data = NULL,
           weight = c("nominal", "ordinal", "interval", "ratio"),
           conf.level = 0.95,
           R = 0) {
    # Prepare
    cl <- match.call()
    na <- method <- nr <- nc <- K <- t <- zero <- est <- mat <- w <- NULL
    list2env(prepd(data, "kra", weight, conf.level), envir = environment())
    
    alfa <- function(data, weight, cl, K) {
      # Contingency table
      mat <- ctab(data, K, cl, zero)
      marg <- rowSums(mat)
      
      # Weight
      w <- wgts(weight, cl, mat, K, zero)
      
      # Point estimate
      if (nrow(mat) == 1 && ncol(mat) == 1) {
        vs <- matrix(c(1, 1), 2, 1)
      } else {
        vs <- combn(1:K, 2)
      }
      est <-
        1 - (sum(marg) - 1) * sum(mat[upper.tri(mat, diag = FALSE)] * w[upper.tri(w, diag =
                                                                                    FALSE)]) /
        sum(marg[vs[1,]] * marg[vs[2,]] * t(w[lower.tri(w, diag = FALSE)]))
      
      return(list(
        'est' = est,
        'mat' = mat,
        'w' = w
      ))
    }
    list2env(alfa(data, weight, "kra", K), envir = environment())
    
    # Bootstrapped confidence intervals
    if (R == 0 || is.nan(est)) {
      cb <- c(NA, NA)
    } else{
      cb <-
        quantile(
          sapply(1:R, function(x)
            alfa(data[sample(nr, replace = TRUE), ], weight, cl, K)),
          probs = c((1 - conf.level) / 2, conf.level + (1 - conf.level) /
                      2),
          na.rm = TRUE
        )
    }
    attr(cb, "names") <- "Const"
    
    # Export
    y <- structure(
      list(
        method = method,
        call = cl,
        obs = nc,
        sample = nr,
        est = est,
        se = NA,
        conf.level = conf.level,
        lb = cb[1],
        ub = cb[2],
        mat = mat,
        weight = w,
        data = data,
        na = na
      ),
      class = c("rel", "kra")
    )
    return(y)
    
  }
