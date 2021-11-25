# RI_Txp.R
tolerance <- 0.01
RI <- 15 # intended RI in seconds
T <- 1 # starting with 1; if necessary, it will decrease to T<1 
p <- seq(0.00001,0.99999,by=0.00001) # possible p (large range)

stepT <- 0.001
ok <- FALSE
while(!ok)
{
  sigma <- T*sqrt((1-p)/(p^2)) # equation (2), see paper
  # select p values inside tolerance margins and get the maximum p
  ps <- p[which(sigma>=(1-tolerance)*RI & sigma<=(1+tolerance)*RI)]
  ps <- max(ps)
  # compute correspondent tolerance
  ToleranceRI <- (T/ps)/RI # equation (1), see paper
  # check if inside margin
  if (ToleranceRI>=(1-tolerance) & ToleranceRI<=(1+tolerance))
  {
    ok <- TRUE # it was found a combination of T and p
  } else
  {
    T <- T - stepT # none was found, try to decrease T
  }    
}
if (ok)
{
  RI <- T/ps # equation (1)
  cat("RI ",RI,": T = ",T," & p = ",ps," (tolerance=",tolerance,")\n",sep="")
} else
{
  cat("No combination of T and p found.\n")
}
