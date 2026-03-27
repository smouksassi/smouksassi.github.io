deltamethod <- function (g, mean, cov, ses = TRUE, envir = parent.frame()) 
{
  cov <- as.matrix(cov)
  n <- length(mean)
  if (!is.list(g)) 
    g <- list(g)
  if ((dim(cov)[1] != n) || (dim(cov)[2] != n)) 
    stop(paste("Covariances should be a ", n, " by ", n, 
               " matrix"))
  syms <- paste("x", 1:n, sep = "")
  for (i in 1:n) assign(syms[i], mean[i])
  gdashmu <- t(sapply(g, function(form) {
    as.numeric(attr(eval(deriv(form, syms), envir = envir), "gradient"))
  }))
  new.covar <- gdashmu %*% cov %*% t(gdashmu)
  if (ses) {
    new.se <- sqrt(diag(new.covar))
    new.se
  }
  else new.covar
}

computerelativeHR <- function (egfrr=70,
                               tkvr=1000,
                               ager=40,
                               egfrt=50,
                               tkvt=1200,
                               aget=40,
                               conf.int=0.95,
                               model = fitSURVfinal
                               )
{ 
  betaesrd = model$coef
  covpesrd = vcov(model, regcoef.only = TRUE, intercepts = "none")
  x1 <- betaesrd[1]
  x2 <- betaesrd[2]
  x3 <- betaesrd[3]
  x4 <- betaesrd[4]
  x5 <- betaesrd[5]
  logHResrd <- (
    ( x1*(egfrt) + x2*( log(tkvt)) + x3*(aget) +
                     x4*egfrt*aget + x5*log(tkvt) * aget  ) -
    ( x1*(egfrr) + x2*( log(tkvr)) + x3*(ager) +
                     x4*egfrr*ager+ x5*log(tkvr) * ager )
                )
  
  HResrd <- exp(logHResrd)
  seloghresrd <- deltamethod(~ (
    ( x1*(egfrt) + x2*( log(tkvt)) + x3*(aget) +
        x4*egfrt*aget + x5*log(tkvt) * aget  ) -
    ( x1*(egfrr) + x2*( log(tkvr)) + x3*(ager) +
          x4*egfrr*ager+ x5*log(tkvr) * ager )
  ), betaesrd, covpesrd )
  
  LOWESRD  <- exp( logHResrd - qnorm((1 + conf.int)/2) *seloghresrd )
  HIGHESRD <- exp( logHResrd + qnorm((1 + conf.int)/2) *seloghresrd )
  
  data.frame(HR=HResrd,LOW= LOWESRD,HIGH=HIGHESRD)
  
}