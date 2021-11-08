bf10_t <- function(t,
                   n1,
                   n2 = NULL,
                   independentSamples = FALSE,
                   prior.location,
                   prior.scale,
                   prior.df,
                   rel.tol = .Machine$double.eps^0.25) {
  
  neff <- ifelse(independentSamples, n1 * n2 / (n1 + n2), n1)
  nu <- ifelse(independentSamples, n1 + n2 - 2, n1 - 1)
  
  mu.delta <- prior.location
  gamma <- prior.scale
  kappa <- prior.df
  numerator <- integrate(integrand_t, lower = -Inf, upper = Inf,
                         t = t, n = neff, nu = nu,
                         mu.delta = mu.delta,
                         gamma = gamma,
                         kappa = kappa,
                         rel.tol = rel.tol)$value
  denominator <- dt(x = t, df = nu)
  
  BF10 <- numerator / denominator
  priorAreaSmaller0 <- pt(q = - mu.delta / gamma, df = kappa)
  postAreaSmaller0 <- cdf_t(x = 0, t = t, n1 = n1, n2 = n2,
                            independentSamples = independentSamples,
                            prior.location = prior.location,
                            prior.scale = prior.scale,
                            prior.df = prior.df,
                            rel.tol = rel.tol)
  BFmin1 <- postAreaSmaller0 / priorAreaSmaller0
  BFplus1 <- (1 - postAreaSmaller0) / (1 - priorAreaSmaller0)
  BFmin0 <- BFmin1 * BF10
  BFplus0 <- BFplus1 * BF10
  
  return(list(BF10 = BF10, BFplus0 = BFplus0, BFmin0 = BFmin0))
  
}