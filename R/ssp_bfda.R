ssp_bfda <- function(tpr = 0.8, delta, thresh = 10, n_rep = 10000, prior_scale = 1 / sqrt(2)) {
  Ns = NULL
  BFs = NULL
  for (i in 1:n_rep) {
    n = 10
    Plac = rnorm(n, 0, 1)
    Treat = rnorm(n, delta, 1)
    BF = BayesFactor::ttest.tstat(
      t = t.test(Treat, Plac)$statistic,
      n1 = n,
      n2 = n, 
      rscale = prior_scale,
      nullInterval = c(0, Inf),
      simple = T)
    
    while (BF > (1 / thresh) & BF < thresh & n < 1500) {
      n = n + 1
      Plac = c(Plac, stats::rnorm(1, 0, 1))
      Treat = c(Treat, stats::rnorm(1, delta, 1))
      BF = BayesFactor::ttest.tstat(
        t = t.test(Treat, Plac)$statistic,
        n1 = n,
        n2 = n, 
        rscale = prior_scale,
        nullInterval = c(0, Inf),
        simple = T)
    }
    Ns[i] = n
    BFs[i] = BF
  }
  
  return(
    list(
      tpr = tpr,
      n1 = round(quantile(Ns, probs = tpr, names = FALSE)),
      h0 = mean(BFs < (1 / thresh)),
      ha = mean(BFs > thresh)
    )
  )
}