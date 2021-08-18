ssp_tost <- function(tpr, eq_band, delta, alpha = .05, max_n = 10001) {
  nr = 1
  sigma = 1
  n1 = 4
  sigsq = sigma^2
  numint = 1000
  lc = numint + 1
  cl = 1e-10
  coevecc = c(1, rep(c(4, 2), numint / 2 - 1), 4, 1)
  tpr_out = 0
  
  while (tpr_out < tpr & n1 < max_n) {
    n1 = n1 + 1
    n2 = nr * n1
    df = n1 + n2 - 2
    tcrit = qt(1 - alpha, df)
    nfac = 1 / n1 + 1 / n2
    var = sigsq * nfac
    std = sqrt(var)
    cu = (df * eq_band^2) / (var * tcrit^2)
    int = cu - cl
    intl = int / numint
    cvec = cl + intl * (0:numint)
    wcpdf = (intl / 3) * coevecc * dchisq(cvec, df)
    st = sqrt(cvec / df) * tcrit
    tpr_out = sum(wcpdf * (pnorm((eq_band - delta) / std - st) - pnorm((-eq_band - delta) / std + st)))
  }
  
  if (tpr_out == 0) {
    stop(paste0("Your chosen power level cannot be achieved for n < ", max_n, "!"))
  } else {
    return(
      list(n1 = round(n1, 4),
           n2 = round(n2, 4),
           tpr_out = round(tpr_out, 4)))
  }
}
