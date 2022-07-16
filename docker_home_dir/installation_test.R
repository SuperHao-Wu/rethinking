library(rethinking)

f <- alist(
  y ~ dnorm( mu , sigma ),
  mu ~ dnorm( 0 , 10 ),
  sigma ~ dexp( 1 )
)

fit <- quap( 
  f , 
  data=list(y=c(-1,1)) , 
  start=list(mu=0,sigma=1)
)
fit_stan <- ulam( f , data=list(y=c(-1,1)) )


fit_stan <- ulam(
  alist(
    y ~ normal( mu , sigma ),
    mu ~ normal( 0 , 10 ),
    sigma ~ exponential( 1 )
  ), data=list(y=c(-1,1)) )


devtools::install_github("RobinHankin/Brobdingnag")
