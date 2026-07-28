meta_ANOVA <- function(
    response, 
    mcse, 
    terms, 
    data,
    check_balance = TRUE
) {
  
  if (!inherits(data, "data.frame")) {
    stop("`data` must be a data.frame.")
  }
  
  if (!is.character(response) || length(response) != 1L || !(response %in% names(data))) {
    stop("`response` must be a character vector of length 1 corresponding to a variable in `data`.")
  }
  
  if (!is.character(mcse) || length(mcse) != 1L || !(mcse %in% names(data))) {
    stop("`mcse` must be a character vector of length 1 corresponding to a variable in `data`.")
  }
  
  if (any(data[mcse] <= 0)) {
    stop("`mcse` includes non-positive values. Monte Carlo errors must be strictly positive.")
  }
  
  if (!is.character(terms) || length(terms) <= 1L) {
    stop("`terms` must be a character vector listing two or more factors.")
  }
  
  missing_terms <- setdiff(terms, names(data))
  if (length(missing_terms) > 0L) {
    stop(paste("The following terms are not in `data`:", paste(missing_terms, collapse = ", ")))
  }
  
  data <- data[c(response,mcse,terms)]
  
  # Ensure all terms are factors
  not_factors <- terms[!sapply(data[terms], is.factor)]
  data[not_factors] <- lapply(data[not_factors], as.factor)
  
  
  # Check for balance
  factor_formula <- stats::reformulate(paste(terms, collapse = " * "), response = response)
  if (check_balance) {
    balance_check <- stats::replications(factor_formula, data = data)
    imbalanced <- is.list(balance_check)
    if (imbalanced) {
      stop("Data are not from a balanced factorial design.")
    }
  }
  
  
  # Analysis of Variance
  
  factor_contrasts <- rep(list("contr.sum"), length(terms))
  names(factor_contrasts) <- terms
  Xmat <- model.matrix(
    factor_formula, 
    data = data, 
    contrasts.arg = factor_contrasts,
    singular.ok = TRUE
  )
  y <- data[[response]]
  mcse <- data[[mcse]]
  
  # Split Xmat by term
  term_IDs <- split(1:ncol(Xmat), attr(Xmat, "assign"))
  X_q <- lapply(term_IDs[-1], \(q) Xmat[,q,drop=FALSE])
  Q <- length(X_q)

  # Sums of squares
  P_q <- lapply(X_q, \(x) qr.Q(qr(x)))
  Pty_q <- lapply(P_q, \(p) colSums(y * p))
  SS_q <- sapply(Pty_q, \(x) sum(x^2))
  
  # Degrees of freedom
  df <- sapply(X_q, NCOL)

  # Adjustment terms  
  P_sigma_q <- lapply(P_q, \(p) mcse * p)
  Omega_q <- lapply(P_sigma_q, crossprod)
  A_q <- sapply(Omega_q, \(om) sum(diag(om)))
  
  # Variance-covariance matrix for SS
  Vmat <- matrix(NA, nrow = Q, ncol = Q)
  for (q in 1:Q) {
    for (r in q:Q) {
      Omega_qr <- crossprod(P_sigma_q[[q]], P_sigma_q[[r]])
      Vmat[q,r] <- sum(Omega_qr^2) + sum(Omega_qr * tcrossprod(Pty_q[[q]], Pty_q[[r]]))
      if (q != r) Vmat[r,q] <- Vmat[q,r] 
    }
  }
  
  # Format as a data.frame
  SS_table <- data.frame(
    term = attr(terms(factor_formula), "term.labels"),
    df = df,
    SS = SS_q,
    A = A_q,
    SSA = SS_q - A_q,
    SSA_mcse = sqrt(diag(Vmat))
  )
  
  attr(SS_table, "vcov") <- Vmat
  return(SS_table)
} 
