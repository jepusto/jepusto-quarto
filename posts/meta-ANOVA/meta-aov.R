meta_aov <- function(
    response, 
    mcse, 
    data,
    check_balance = TRUE,
    test = FALSE
) {
  
  if (!inherits(data, "data.frame")) {
    stop("`data` must be a data.frame.")
  }
  
  #-------------------------------------------------------------
  # Ensure all terms are factors
  
  terms <- rownames(attr(terms(response), "factors"))[-1]
  not_factors <- terms[!sapply(data[terms], is.factor)]
  data[not_factors] <- lapply(data[not_factors], as.factor)
  
  
  #-------------------------------------------------------------
  # make the model.frame
  
  mf <- match.call()
  m <- match(c("response", "mcse", "data"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf[[1L]] <- quote(stats::model.frame)
  names(mf)[2] <- "formula"
  mf <- eval(mf, parent.frame())
  mt <- attr(mf, "terms")
  y <- model.response(mf, "numeric")
  mcse <- model.extract(mf, "mcse")
  
  
  #-------------------------------------------------------------
  # Check for balance
  
  if (check_balance) {
    balance_check <- stats::replications(mt, data = data)
    imbalanced <- is.list(balance_check)
    if (imbalanced) {
      stop("Data are not from a balanced factorial design.")
    }
  }
  
  
  #-------------------------------------------------------------
  # Analysis of Variance
  
  # get the full X matrix
  factor_contrasts <- rep(list("contr.sum"), length(terms))
  names(factor_contrasts) <- terms
  Xmat <- model.matrix(
    mt, 
    data = mf, 
    contrasts.arg = factor_contrasts,
    singular.ok = TRUE
  )
  
  # Split Xmat by term
  term_labs <- attr(mt, "term.labels")
  term_IDs <- split(1:ncol(Xmat), attr(Xmat, "assign"))
  X_q <- lapply(term_IDs, \(q) Xmat[,q,drop=FALSE])
  Q <- length(X_q)

  # Sums of squares
  P_q <- lapply(X_q, \(x) qr.Q(qr(x)))
  b_q <- lapply(P_q, \(p) colSums(y * p))
  SS_q <- sapply(b_q, \(x) sum(x^2))
  
  # N and degrees of freedom
  N <- nrow(Xmat)
  df <- sapply(X_q, NCOL)

  # Adjustment terms  
  P_sigma_q <- lapply(P_q, \(p) mcse * p)
  Omega_q <- lapply(P_sigma_q, crossprod)
  A_q <- sapply(Omega_q, \(om) sum(diag(om)))
  
  # Variance-covariance matrix for SS
  Omega_tr <- matrix(NA, nrow = Q, ncol = Q)
  Vmat <- matrix(NA, nrow = Q, ncol = Q)
  for (q in 1:Q) {
    for (r in q:Q) {
      Omega_qr <- crossprod(P_sigma_q[[q]], P_sigma_q[[r]])
      Omega_tr[q,r] <- sum(Omega_qr^2)
      Vmat[q,r] <- 2 * Omega_tr[q,r]  + 4 * sum(Omega_qr * tcrossprod(b_q[[q]], b_q[[r]]))
      if (q != r) {
        Omega_tr[r,q] <- Omega_tr[q,r]
        Vmat[r,q] <- Vmat[q,r] 
      }
    }
  }
  
  #-------------------------------------------------------------
  # Compute SS Residual if present
  
  df_mod <- sum(df)
  
  if (df_mod < N) {
    term_labs <- c(term_labs, "Residuals")
    df <- c(df, N - df_mod)
    
    SST <- sum(y^2)
    SSR <- SST - sum(SS_q)
    SS_q <- c(SS_q, SSR)
    mcse2 <- mcse^2
    A_R <- sum(mcse2) - sum(A_q)
    A_q <- c(A_q, A_R)
    
    # Compute residuals
    Xtheta_q <- mapply(\(p,b) p %*% b, p = P_q, b = b_q, SIMPLIFY = TRUE)
    resid <- y - rowSums(Xtheta_q)
    Sigma_r <- mcse2 * resid
    
    # Compute variance and covariances
    mcse4 <- mcse^4
    tr_P_Sigma2_P_q <- sapply(P_q, \(p) sum(mcse4 * p^2))
    
    Omega_tr_R <- sum(mcse4) - 2 * sum(tr_P_Sigma2_P_q) + sum(Omega_tr)
    Omega_tr_Rq <- tr_P_Sigma2_P_q - colSums(Omega_tr)
    Omega_tr <- rbind(cbind(Omega_tr, Omega_tr_Rq), c(Omega_tr_Rq, Omega_tr_R))
    
    V_SSR <- 2 * Omega_tr_R + 4 * sum(resid * Sigma_r)
    Cov_SSR <- 2 * Omega_tr_Rq + 4 * as.numeric(crossprod(Sigma_r, Xtheta_q))
        Vmat <- rbind(cbind(Vmat, Cov_SSR), c(Cov_SSR, V_SSR))
  }
  
  #-------------------------------------------------------------
  # Format the ANOVA table
  
  SS_table <- data.frame(
    term = term_labs,
    df = df[-1],
    SS = SS_q[-1],
    A = A_q[-1],
    SSA = SS_q[-1] - A_q[-1],
    SSA_mcse = sqrt(diag(Vmat)[-1]),
    row.names = NULL
  )
  
  #-------------------------------------------------------------
  # Compute hypothesis tests if so specified
  
  if (test) {
    C_q <- SS_q / A_q
    nu_q <- A_q^2 / diag(Omega_tr)
    pval_q <- pchisq(C_q, df = nu_q, lower.tail = FALSE)
  
    SS_table$C_q <- C_q[-1]
    SS_table$nu <- nu_q[-1]
    SS_table$pval <- pval_q[-1]
  }


  #-------------------------------------------------------------
  # Store variance-covariance matrix and null vcov matrix 
  
  Vmat <- Vmat[-1,-1,drop=FALSE]
  colnames(Vmat) <- rownames(Vmat) <- term_labs
  attr(SS_table, "vcov") <- Vmat
  
  Omega_tr <- Omega_tr[-1,-1,drop=FALSE]
  colnames(Omega_tr) <- rownames(Omega_tr) <- term_labs
  attr(SS_table, "Omega_tr") <- Omega_tr
  
  return(SS_table)
} 
