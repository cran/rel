"prepd" <-
  function(data, cl, weight, conf.level, kat = NULL) {
    
    # Data
    if (any(grepl('^kra$', cl))){
      data <- as.matrix(data)
    } else{
      data <- as.matrix(na.omit(data))
    }
    nr <- nrow(data)
    nc <- ncol(data)
    t <- qt(1 - (1 - conf.level) / 2, nr - 1)
    zero <- min(data, na.rm=TRUE) == 0

    # Number of categories
    if (is.character(data)) {
      na <- sum(grepl('^$', data)) / (nr * nc) * 100
      data <- matrix(as.numeric(as.factor(data)), nr, nc)
      K <- max(data, na.rm = TRUE)
    } else{
      na <- sum(is.na(data)) / (nr * nc) * 100
      K <- ifelse(any(grepl('^gac$|^bags$', cl)) & is.numeric(kat),
                  kat,
                  max(length(
                    min(data, na.rm = TRUE):max(data, na.rm = TRUE)
                  )))
    }
    
    # Method
    if (any(grepl('^bags$', cl))) {
      method <- "Bennett et als S"
    } else if (any(grepl('^ckap$', cl))) {
      method <-
        paste0(ifelse(is.numeric(weight), "custom-weighted", weight), ' kappa')
    } else if (any(grepl('^gac$', cl))) {
      method <-
        paste0(ifelse(is.numeric(weight), "custom-weighted", weight),
               ifelse(grep("^unweighted$", weight), " AC1", " AC2"))
    } else if (any(grepl('^kra$', cl))) {
      method <-
        paste0("Krippendorf's alpha with ",
               ifelse(is.numeric(weight), "custom", weight),
               " weight")
    } else{
      method <-
        paste0(ifelse(is.numeric(weight), "custom-weighted", weight), ' pi')
    }
    
    return(list(
      'data' = data,
      'na' = na,
      'method' = method,
      'nr' = nr,
      'nc' = nc,
      'K' = K,
      't' = t,
      'zero' = zero
    ))
    
  }



