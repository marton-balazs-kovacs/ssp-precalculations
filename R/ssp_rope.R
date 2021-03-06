ssp_rope <- function(tpr, eq_band, delta) {
  est <- ssp_tost(tpr = tpr, eq_band = eq_band, delta = delta)[[1]]
  result <- power_optim(fun = rope, range = round(est * c(0.5, 1.5)), delta = delta, tpr = tpr, eq_band = eq_band)
  result
}

rope <- function(n1, delta, eq_band, alpha = .05, tol = 1e-4, granularity = 300) {
  n2 = n1
  t = seq(qt(.005, n1+n2-2, delta/sqrt(1/n1+1/n2)), 
          qt(.995, n1+n2-2, delta/sqrt(1/n1+1/n2)), length = granularity)
  upper = 0.5
  lower = 0.5
  i = 0
  while (!(lower<(alpha/2) & upper>(1-alpha/2)) & i<granularity) {
    i = i+1
    upper = cdf_t(eq_band, t[i], n1 = n1, n2 = n2, independentSamples = T, prior.location = 0, 
                  prior.scale = 1/sqrt(2), prior.df = 1, rel.tol = tol)
    lower = cdf_t(-eq_band, t[i], n1 = n1, n2 = n2, independentSamples = T, prior.location = 0, 
                  prior.scale = 1/sqrt(2), prior.df = 1, rel.tol = tol)
  }
  if (i==granularity) {
    npower = 0
  } else {
    upper = 0.5
    lower = 0.5
    j = length(t) + 1
    
    while (!(lower<(alpha/2) & upper>(1-alpha/2))) {
      j = j-1
      upper = cdf_t(eq_band, t[j], n1 = n1, n2 = n2, independentSamples = T, prior.location = 0, 
                    prior.scale = 1/sqrt(2), prior.df = 1, rel.tol = tol)
      lower = cdf_t(-eq_band, t[j], n1 = n1, n2 = n2, independentSamples = T, prior.location = 0, 
                    prior.scale = 1/sqrt(2), prior.df = 1, rel.tol = tol)
    }
    
    npower = pt(t[j], n1+n2-2, delta/sqrt(1/n1+1/n2)) - pt(t[i], n1+n2-2, delta/sqrt(1/n1+1/n2))
  }
  npower
}
