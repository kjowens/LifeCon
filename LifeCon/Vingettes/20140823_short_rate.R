library("sde")
myparam <- ASMparam2SDE(a=.5, b=.1, sigma=.12, phi=.25)
mytheta <- myparam$theta
mytheta
r0 <- 0.08
r0
h <- 1
my_n <- 30

set.seed(5)
my_rs <- c(r0,rcCIR(n=my_n,Dt=h, x0=r0, theta=mytheta))
# plot(my_rs)
# summary(my_rs)

my5yrpv <- function(rate, ...) {zcb.cir(param=myparam, sr=rate, tstart=0, tend=5)$pv}
# my5yrpv(.08)
# undebug(zcb.cir)
# zcb.cir(param=myparam, sr=r0, tstart=0, tend=5)

my_prices <- mapply(my5yrpv, my_rs)
plot(my_prices)