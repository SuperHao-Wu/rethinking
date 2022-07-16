library(rethinking)
data(chimpanzees)
d <- chimpanzees

d$treatment <- 1 + d$prosoc_left + 2*d$condition

xtabs( ~ treatment + prosoc_left + condition , d )

m11.1 <- quap(
               alist(
                 pulled_left ~ dbinom( 1 , p ) ,
                 logit(p) <- a ,
                 a ~ dnorm( 0 , 10 )
               ) , data=d )
