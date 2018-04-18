generate_data = function(n, p) {
  covariates = matrix(rnorm(n*p), nrow=n, ncol=p)
  responses = rnorm(n)
  return(list(covariates=covariates, responses=responses))
}

model_select = function(covariates, responses, cutoff) {
  all = lm(responses ~ covariates)
  coefs = coef(all)[-1] # remove intercept
  cov.reduced.ind = which(coefs <= cutoff)
  if (length(cov.reduced.ind) == 0) return(c())
  else {
    reduced.model = lm(responses ~ covariates[,cov.reduced.ind])
    coefs.reduced.model = coef(reduced.model)
    return(coefs.reduced.model)
  }
}
