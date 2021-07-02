ssp_eq_bf <- function(tpr, eq_band, delta, thresh = 10, tol = 1e-4, granularity = 300, prior_location = 0, prior_scale = 1/sqrt(2)) {
  est <- ssp_tost(tpr = tpr, eq_band = eq_band, delta = delta) %>% purrr::pluck("n1")
  result <- power_optim(
    fun = eq_bf,
    range = c(10, est),
    delta = delta,
    tpr = tpr,
    eq_band = eq_band,
    thresh = thresh,
    tol = tol,
    granularity = granularity,
    prior_location = prior_location,
    prior_scale = prior_scale)
  result
}

eq_bf <- function(n1, eq_band, delta, thresh, tol, granularity, prior_location, prior_scale) {
  n2 = n1
  t = seq(stats::qt(.005, n1+n2-2, delta/sqrt(1/n1+1/n2)), 
          stats::qt(.995, n1+n2-2, delta/sqrt(1/n1+1/n2)), length = granularity)
  bf = 1
  i = 0
  while (bf < thresh & i < granularity) {
    i = i + 1
    upper = cdf_t(eq_band, t[i], n1 = n1, n2 = n2, independentSamples = T, prior.location = prior_location, 
                  prior.scale = prior_scale, prior.df = 1, rel.tol = tol)
    lower = cdf_t(-eq_band, t[i], n1 = n1, n2 = n2, independentSamples = T, prior.location = prior_location, 
                  prior.scale = prior_scale, prior.df = 1, rel.tol = tol)
    post_dens = upper - lower
    prior_dens = stats::pcauchy(eq_band, location = prior_location, scale = prior_scale) - stats::pcauchy(-eq_band, location = prior_location, scale = prior_scale)
    bf = (post_dens / prior_dens) / ((1 - post_dens) / (1 - prior_dens))
  }
  if (i==granularity) {
    npower = 0
  } else {
    bf = 1
    j = length(t) + 1
    while (bf < thresh) {
      j = j-1
      upper = cdf_t(eq_band, t[j], n1 = n1, n2 = n2, independentSamples = T, prior.location = prior_location, 
                    prior.scale = prior_scale, prior.df = 1, rel.tol = tol)
      lower = cdf_t(-eq_band, t[j], n1 = n1, n2 = n2, independentSamples = T, prior.location = prior_location, 
                    prior.scale = prior_scale, prior.df = 1, rel.tol = tol)
      post_dens = upper - lower
      prior_dens = stats::pcauchy(eq_band, location = prior_location, scale = prior_scale) - stats::pcauchy(-eq_band, location = prior_location, scale = prior_scale)
      bf = (post_dens / prior_dens) / ((1 - post_dens) / (1 - prior_dens))
    }
    npower = stats::pt(t[j], n1+n2-2, delta/sqrt(1/n1+1/n2)) - stats::pt(t[i], n1+n2-2, delta/sqrt(1/n1+1/n2))
  }
  npower
}