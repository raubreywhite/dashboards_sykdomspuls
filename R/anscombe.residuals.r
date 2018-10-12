anscombe.residuals <- function(m,phi) {
  y <- m$y
  mu <- fitted.values(m)
  #Compute raw Anscombe residuals
  a <- 3/2*(y^(2/3) * mu^(-1/6) - mu^(1/2))
  #Compute standardized residuals
  a <- a/sqrt(phi * (1-hatvalues(m)))
  return(a)
}
