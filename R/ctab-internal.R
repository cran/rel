"ctab" <-
  function(data, K, cl, zero) {
    
    if (zero == TRUE) {
      data <- data + 1
    }
    
    if (any(!grepl("^kra$", cl))) {
      mat <- matrix(0, K, K)
      obs <- table(data[, 1], data[, 2])
      mat[as.numeric(rownames(obs)), as.numeric(colnames(obs))] <-
        obs
    } else {
      mu <- rowSums(!is.na(data))
      ap <- expand.grid(seq_len(ncol(data)), seq_len(ncol(data)))
      ap <- ap[ap[, 1] != ap[, 2], ]
      tab <-
        quote(na.omit(table(data[x, ap[, 1]], data[x, ap[, 2]]) / (mu[x] - 1)))
      corr <- function(x) {
        mat <- matrix(0, K, K)
        mat[as.numeric(rownames(eval(tab))), as.numeric(colnames(eval(tab)))] <-
          eval(tab)
        return(mat)
      }
      mat <- Reduce("+", lapply(X = 1:nrow(data), corr))
    }
    return(mat)
  }
