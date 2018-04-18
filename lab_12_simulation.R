generate_data = function(n, p) {
  covariates = matrix(rnorm(n*p), nrow=n, ncol=p)
  responses = rnorm(n)
  return(list(covariates=covariates, responses=responses))
}

model_select = function(covariates, responses, cutoff) {
  all.lm = lm(responses ~ covariates)
  pval.all = summary(all.lm)$coefficients[,4][-1] # remove intercept
  cov.reduced.ind = which(pval.all <= cutoff)
  if (length(cov.reduced.ind) == 0) return(c())
  else {
    reduced.lm = lm(responses ~ covariates[,cov.reduced.ind])
    pval.reduced = summary(reduced.lm)$coefficients[,4]
    return(pval.reduced)
  }
}

single.sim = function(n, p, cutoff) {
  data = generate_data(n, p)
  return(model_select(data$covariates, data$responses, cutoff))
}

run_simulation = function(n_trials, n, p, cutoff) {
  sim.objs = replicate(n=n_trials, expr=(single.sim(n, p, cutoff)))
  sim.objs = unlist(sim.objs)
  hist(sim.objs, xlab=paste("p-values"), main=paste("Histogram of p-values with \nn = ", n, "p = ", p))
}

for (i in c(100, 1000, 10000)) {
  mapply(FUN=run_simulation, n_trials=100, n=i, p=c(10, 20, 50), cutoff=0.5)
}




