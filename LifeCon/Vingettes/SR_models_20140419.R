librrary("sde")

# OU (Ornstien-Ulenbak), or Vasicek model

## translating ASM parameterization to sde package params
ou.a <- .2
ou.b <- .1
ou.vol <- .1
theta1 <- ou.b*ou.a
theta2 <- ou.a
theta3 <- ou.vol
theta <- c(theta1, theta2, theta3)
phi <- .15

r0 <- 0.02 # setting a short rate at time zero

ou.rbar <- function(theta, phi) {
	theta[1]/theta[2] + theta[3]*phi/theta[2] - 0.5*(theta[3]/theta[2])^2
}

ou.B <- function(theta, tstart, tend) {
	out <- ( 1 - exp(-theta[2]*(tend - tstart)) ) / theta[2]
	return(out)
}

ou.A <- function(theta, phi, tstart, tend) {
	out <- NULL
	rbar <- ou.rbar(theta, phi)
	B <- ou.B(theta, tstart, tend)
	first <- rbar*(B + tstart - tend)
	second <- (B*theta[3])^2 / (4*theta[2])
	out <- exp(first - second)
	return(out)
}

zcb.ou.pv <- function(sr, theta, phi, tstart, tend) {
	out <- NULL
	B <- ou.B(theta, tstart, tend)
	A <- ou.A(theta, phi, tstart, tend)
	out <- A*exp(-B*sr)
	return(out)
}

zcb.ou.ytm <- function(sr, theta, phi, tstart, tend) {
	out <- NULL
	rbar <- ou.rbar(theta, phi)
	B <- ou.B(theta, tstart, tend)
	temp <- -rbar*B + (B*theta[3])^2 / (4*theta[2]) + B*sr
	out <- rbar + (temp)/(tend - tstart)
	return(out)
}

theta

ou.rbar(theta, phi)
ou.B(theta, 0, 10)
ou.A(theta, phi, 0, 10)
zcb.ou.pv(r0, theta, phi, 0, 10)
zcb.ou.ytm(r0, theta, phi, 0, 10)


# fixed coupon bond
fcb <- list()
fcb$par <- 100
fcb$coup <- .04
fcb$freq <- 2
fcb$issuedate <- 2015
fcb$matdate <- 2025
fcb$cfdf <- data.frame(dates=seq(fcb$issuedate + 1/fcb$freq, fcb$matdate, 1/fcb$freq)) # not including zero
fcb$cfdf$int <- rep(fcb$par*fcb$coup/fcb$freq, length(fcb$cfdf[1])) # interest
fcb$cfdf$pri <- ifelse(fcb$cfdf$dates==fcb$matdate,1,0)*fcb$par # principal
fcb$cfdf$cfs <- fcb$cfdf$int + fcb$cfdf$pri

currdate <- 2015

pv.ou.fcb <- function(fcb, sr, theta, phi, t) {
	out <- NULL
	if(t>fcb$matdate) {
		out <- 0
		} else {
			mydates <- fcb$cfdf$dates
			mycfs <- fcb$cfdf$cfs
			incl <- ifelse(mydates>t,1,0) # don't incl pmnt at valuation date
			dum <- function(x) {zcb.ou.pv(sr, theta, phi, tstart=t, x)}
			disc <- vapply(mydates,1,FUN=dum)
			out <- sum(incl * disc * mycfs)
		};
	return(out)
}

fcb$pv <- pv.ou.fcb(fcb, r0, theta, phi, t=currdate)
fcb$pv

library(lifecontingencies)
getirr <- function(p) {
	(presentValue(cashFlows=fcb$cfdf$cfs, timeIds=(fcb$cfdf$dates - fcb$issuedate),interestRates=p) - fcb$pv)^2
}
fcb$ytm <- nlm(f=getirr, p=.1)$estimate