ssp_infer_bf <- function(tpr, ni_margin, delta, thresh = 10, tol = 1e-4, granularity = 300, prior_location = 0, prior_scale = 1/sqrt(2), max_n = 10001) {
  result <- tpr_optim(
    fun = infer_bf,
    range = c(5, max_n),
    delta = delta,
    tpr = tpr,
    ni_margin = ni_margin,
    thresh = thresh,
    tol = tol,
    granularity = granularity,
    prior_location = prior_location,
    prior_scale = prior_scale)
  result
}

infer_bf <- function(n1, ni_margin, delta, thresh, tol, granularity, prior_location, prior_scale) {
  n2 = n1
  t = seq(stats::qt(.005, n1+n2-2, (delta+ni_margin)/sqrt(1/n1+1/n2)), 
          stats::qt(.995, n1+n2-2, (delta+ni_margin)/sqrt(1/n1+1/n2)), length = granularity)
  bf = 1
  i = 0
  while (bf < thresh & i < granularity) {
    i = i + 1
    res <- bf10_t(t = t[i], n1 = n1, n2 = n2, independentSamples = TRUE, prior.location = prior_location, prior.scale = prior_scale, prior.df = 1, rel.tol = tol)
    bf <- res$BFplus0 / res$BFmin0
  }
  if (i == granularity) {
    tpr_out = 0
  } else {
    bf = 1
    j = length(t) + 1
    while (bf < thresh) {
      j = j-1
      res <- bf10_t(t = t[j], n1 = n1, n2 = n2, independentSamples = TRUE, prior.location = prior_location, prior.scale = prior_scale, prior.df = 1, rel.tol = tol)
      bf <- res$BFplus0 / res$BFmin0
    }
    tpr_out = stats::pt(t[j], n1+n2-2, (delta+ni_margin)/sqrt(1/n1+1/n2)) - stats::pt(t[i], n1+n2-2, (delta+ni_margin)/sqrt(1/n1+1/n2))
  }
  tpr_out
}