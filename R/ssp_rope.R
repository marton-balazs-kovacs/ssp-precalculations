ssp_rope <- function(tpr, eq_band, delta, alpha = .05, tol = 1e-4, granularity = 300, prior_location = 0, prior_scale = 1/sqrt(2)) {
  est <- ssp_tost(tpr = tpr, eq_band = eq_band, delta = delta) %>% purrr::pluck("n1")
  result <- tpr_optim(
    fun = rope,
    range = round(est * c(0.5, 1.5)),
    delta = delta,
    tpr = tpr,
    eq_band = eq_band,
    alpha = alpha,
    tol = tol,
    granularity = granularity,
    prior_location = prior_location,
    prior_scale = prior_scale)
  result
}

rope <- function(n1, delta, eq_band, alpha, tol, granularity, prior_location, prior_scale) {
  n2 = n1
  t = seq(stats::qt(.005, n1+n2-2, delta/sqrt(1/n1+1/n2)), 
          stats::qt(.995, n1+n2-2, delta/sqrt(1/n1+1/n2)), length = granularity)
  upper = 0.5
  lower = 0.5
  i = 0
  while (!(lower<(alpha/2) & upper>(1-alpha/2)) & i < granularity) {
    i = i+1
    upper = cdf_t(eq_band, t[i], n1 = n1, n2 = n2, independentSamples = T, prior.location = prior_location, 
                  prior.scale = prior_scale, prior.df = 1, rel.tol = tol)
    lower = cdf_t(-eq_band, t[i], n1 = n1, n2 = n2, independentSamples = T, prior.location = prior_location, 
                  prior.scale = prior_scale, prior.df = 1, rel.tol = tol)
  }
  if (i == granularity) {
    tpr_out = 0
  } else {
    upper = 0.5
    lower = 0.5
    j = length(t) + 1
    
    while (!(lower<(alpha/2) & upper>(1-alpha/2))) {
      j = j-1
      upper = cdf_t(eq_band, t[j], n1 = n1, n2 = n2, independentSamples = T, prior.location = prior_location, 
                    prior.scale = prior_scale, prior.df = 1, rel.tol = tol)
      lower = cdf_t(-eq_band, t[j], n1 = n1, n2 = n2, independentSamples = T, prior.location = prior_location, 
                    prior.scale = prior_scale, prior.df = 1, rel.tol = tol)
    }
    
    tpr_out = stats::pt(t[j], n1+n2-2, delta/sqrt(1/n1+1/n2)) - stats::pt(t[i], n1+n2-2, delta/sqrt(1/n1+1/n2))
  }
  tpr_out
}
