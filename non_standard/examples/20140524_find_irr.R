

get.irr.gerber <- function(times, spot.rates, pmnts, tol=1e-5, max.iterations=1e4) {
	# calculates internal rate of return
	# based on Gerber, "Life Insurance Mathematics", 3rd edition, page 13
	out <- numeric(1)
	cur.low <- min(spot.rates)
	cur.high <- max(spot.rates)
	# some helper functions, they could be pulled out to use for other things
		pv.spot <- function(times, spot.rates, pmnts) {
			sum(pmnts*exp(-times*spot.rates))
		}
		pv.yield <- function(times, yield, pmnts) {
			sum(pmnts*exp(-times*yield))
		}
		temp.denom <- function(rate.k, ...) {
			log(pv.yield(times, yield=rate.k, pmnts)/sum.pmnts)
		}
		get.next.rate <- function(rate.k, ...) {
			temp.numerator * rate.k / temp.denom(rate.k)
		}
	# setup constants
		my.pv <- pv.spot(times, spot.rates, pmnts)
		sum.pmnts <- sum(pmnts)
		temp.numerator <- log(my.pv/sum.pmnts)
		cur.iter <- 0
	# run interation
	while(all(abs(cur.low-cur.high)>tol, cur.iter<max.iterations)){
		cur.low <- get.next.rate(rate.k=cur.low)
		cur.high <- get.next.rate(rate.k=cur.high)
		cur.iter <- cur.iter + 1
	}
	out <- mean(c(cur.low,cur.high))
	return(out)
}

times <- 1:2
spot.rates <- c(.04, .06) # continuously compounded
pmnts <- c(100, 200)
gerber.est <- get.irr.gerber(times, spot.rates, pmnts)
gerber.est
# validate

get.irr.nlm <- function(times, spot.rates, pmnts) {
	# some helper functions, they could be pulled out to use for other things
		pv.spot <- function(times, spot.rates, pmnts) {
			sum(pmnts*exp(-times*spot.rates))
		}
		pv.yield <- function(times, yield, pmnts) {
			sum(pmnts*exp(-times*yield))
		}
		sqtomin <- function(y, .times=times, .spot.rates=spot.rates, .pmnts=pmnts) {
			(pv.yield(times=.times, yield=y, pmnts=.pmnts) - 
				pv.spot(times=.times, spot.rates=.spot.rates, pmnts=.pmnts))^2
		}
	guess <- mean(spot.rates)
	return(nlm(f=sqtomin, p=guess)$estimate)
}

nlm.est <- get.irr.nlm(times, spot.rates, pmnts)
nlm.est
gerber.est - nlm.est

# test effeciency vs nlm function
library(rbenchmark)
set.seed(42)
times <- 1:361
spot.rates <- .001 + .01*sqrt(times)
pmnts <- 100*runif(length(times))
benchmark(get.irr.gerber(times, spot.rates, pmnts), get.irr.nlm(times, spot.rates, pmnts))


trials <- 50
res <- numeric(trials)
for (i in 1:trials) {
	times <- 1:361
	spot.rates <- .001 + .01*sqrt(times)
	pmnts <- 100*runif(length(times))
	res[i] <- benchmark(get.irr.gerber(times, spot.rates, pmnts), get.irr.nlm(times, spot.rates, pmnts))[2,]$relative
}
summary(res)
# a pretty decent speed up, I'm seeing an average of about 4.5 times faster
