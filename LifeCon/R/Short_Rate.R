library("sde")

# OU (Ornstien-Ulenbak), or Vasicek model


ASMparam2SDE <- function(a, b, sigma, phi=NA) {
  # converts the ASM parameterization to that used in the SDE package
  # returns a list with the parameters needed to simulate "theta" and the constant phi
  theta1 <- b*a
  theta2 <- a
  theta3 <- sigma
  theta <- c(theta1, theta2, theta3)
  out <- list(theta=theta, phi=phi)
  return(out)
}

ou.rbar <- function(.param) {
  # yield on an infinitely lived ZCB
  theta <- .param$theta
  phi <- .param$phi
	theta[1]/theta[2] + theta[3]*phi/theta[2] - 0.5*(theta[3]/theta[2])^2
}

ou.B <- function(theta, tstart, tend) {
	out <- ( 1 - exp(-theta[2]*(tend - tstart)) ) / theta[2]
	return(out)
}

ou.A <- function(param, tstart, tend, B=NA) {
	out <- NULL
  theta <- param$theta
	rbar <- ou.rbar(param)
  if(is.na(B)) {
    B <- ou.B(theta, tstart, tend)
  }
	first <- rbar*(B + tstart - tend)
	second <- (B*theta[3])^2 / (4*theta[2])
	out <- exp(first - second)
	return(out)
}

zcb.ou.pv <- function(sr, theta, phi, tstart, tend) {
	out <- NULL
	B <- ou.B(theta, tstart, tend)
	A <- ou.A(theta, phi, tstart, tend, B)
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

zcb.ou <- function(param, sr=NA, tstart=NA, tend=NA, convert_param=FALSE) {
  # returns a list of various things you might want for a ZCB
  out <- list()
  
  # dots to avoid confusion about passing arguments to other functions
  .param <- param
  .sr <- sr
  .tstart <- tstart
  .tend <- tend
  
  if(convert_param==TRUE) {
    .param <- ASMparam2SDE(a=.param$a, b=.param$b, sigma=.param$sigma, phi=.param$phi)
  }
  
  .theta <- .param$theta
  .phi <- .param$phi
  
  B <- ou.B(.theta, .tstart, .tend)
  A <- ou.A(.param, .tstart, .tend)
  
  pv <- A*exp(-B * .sr)
  ytm <- -log(pv)/(.tend - .tstart)
  
  # denominator in Sharpe ratio on ZCB from t to T given short-rate r
  q_rtT <- B*.theta[3]
  
  Sharpe_ratio <- .phi
  
  # continuous instantanious return on a ZCB from t to T given short-rate r
  alpha_rtT <- .phi * q_rtT + .sr
  
  # partial derivative of ZCB bond price with respect to time
  P_t <- pv*( alpha_rtT + .theta[1] - .theta[2]*.sr - 0.5*(.theta[3]*B)^2)
  
  out$model <- "ou, Vasicek"
  out$param <- .param
  out$rbar <- ou.rbar(.param)
  out$B <- B
  out$A <- A
  out$pv <- pv
  out$ytm <- ytm
  out$alpha_rtT <- alpha_rtT
  out$q_rtT <- q_rtT
  out$Sharpe_ratio <- Sharpe_ratio
  out$P_t <- P_t
  return(out)
}



remaining_cfs <- function(date, obj) {
  # now excludes row if date on row equals valuation date
  obj <- obj[which(obj$dates>date),]
  obj$times <- obj$dates - date
  return(obj)
}

zcbpv <- function(time, param, sr) {zcb.ou(param, sr, tstart=0, tend=time)$pv}



# CIR model
# ASMparam2SDE works the same way, though the terminology in ASM is a little different
# instead of sigma and phi it's sigma-bar and phi-bar, respectively

cir.gamma <- function(.param) {
  theta <- .param$theta
  phi <- .param$phi
  return(  sqrt( (theta[1] - phi)^2 + 2*theta[3]^2 )  )
}

cir.rbar <- function(.param) {
  theta <- .param$theta
  phi <- .param$phi
  gamma <- cir.gamma(.param)
  return(2*theta[1]/(theta[2] - phi + gamma))
}

cir.B <- function(.param, tstart, tend) {
  out <- numeric(0)
  theta <- .param$theta
  phi <- .param$phi
  gamma <- cir.gamma(.param)
  temp1 <- exp(gamma*(tend-tstart)) - 1
  temp2 <- (theta[2] - phi + gamma)
  out <- 2*temp1/(temp2*temp1 + 2*gamma)
  return(out)
}

cir.A <- function(.param, tstart, tend) {
  out <- numeric(0)
  theta <- .param$theta
  phi <- .param$phi
  gamma <- cir.gamma(.param)
  temp1 <- exp(gamma*(tend-tstart)) - 1
  temp2 <- (theta[2] - phi + gamma)
  denominator <- temp1*temp2 + 2*gamma
  numerator <- 2*gamma*exp(0.5*temp2*(tend-tstart))
  exponent <- 2*theta[1]/theta[3]^2
  out <- (numerator/denominator)^exponent
  return(out)
}

zcb.cir.pv <- function(sr, .param, tstart, tend) {
  out <- numeric(0)
  B <- cir.B(.param, tstart, tend)
  A <- cir.A(.param, tstart, tend)
  out <- A*exp(-B*sr)
  return(out)
}

zcb.cir <- function(param, sr=NA, tstart=NA, tend=NA, convert_param=FALSE) {
  # returns a list of various things you might want for a ZCB
  out <- list()
  
  # dots to avoid confusion about passing arguments to other functions
  .param <- param
  .sr <- sr
  .tstart <- tstart
  .tend <- tend
  
  if(convert_param==TRUE) {
    .param <- ASMparam2SDE(a=.param$a, b=.param$b, sigma=.param$sigma, phi=.param$phi)
  }
  
  .theta <- .param$theta
  .phi <- .param$phi
  
  B <- cir.B(.param, tstart, tend)
  A <- cir.A(.param, tstart, tend)
  
  pv <- A*exp(-B * .sr)
  ytm <- -log(pv)/(.tend - .tstart)
  
  # denominator in Sharpe ratio on ZCB from t to T given short-rate r
  q_rtT <- B*.theta[3]*sqrt(.sr)
  
  Sharpe_ratio <- .phi*sqrt(.sr)/.theta[3]
  
  # continuous instantanious return on a ZCB from t to T given short-rate r
  alpha_rtT <- .phi * q_rtT + .sr
  
  # partial derivative of ZCB bond price with respect to time
  P_t <- pv*( alpha_rtT + .theta[1] - .theta[2]*.sr - 0.5*.sr*(.theta[3]*B)^2)
  
  out$model <- "CIR"
  out$param <- .param
  out$rbar <- ou.rbar(.param)
  out$B <- B
  out$A <- A
  out$pv <- pv
  out$ytm <- ytm
  out$alpha_rtT <- alpha_rtT
  out$q_rtT <- q_rtT
  out$Sharpe_ratio <- Sharpe_ratio
  out$P_t <- P_t
  return(out)
}

# delta and duration hedging
# risk neutral pricing


# # fixed coupon bond
# fcb <- list()
# fcb$par <- 100
# fcb$coup <- .04
# fcb$freq <- 2
# fcb$issuedate <- 2015
# fcb$matdate <- 2025
# fcb$cfdf <- data.frame(dates=seq(fcb$issuedate + 1/fcb$freq, fcb$matdate, 1/fcb$freq)) # not including zero
# fcb$cfdf$int <- rep(fcb$par*fcb$coup/fcb$freq, length(fcb$cfdf[1])) # interest
# fcb$cfdf$pri <- ifelse(fcb$cfdf$dates==fcb$matdate,1,0)*fcb$par # principal
# fcb$cfdf$cfs <- fcb$cfdf$int + fcb$cfdf$pri
# 
# mapply(zcb.ou)
# help("mapply")
# 
# currdate <- 2015
# 
# pv.ou.fcb <- function(fcb, sr, theta, phi, t) {
# 	out <- NULL
# 	if(t>fcb$matdate) {
# 		out <- 0
# 		} else {
# 			mydates <- fcb$cfdf$dates
# 			mycfs <- fcb$cfdf$cfs
# 			incl <- ifelse(mydates>t,1,0) # don't incl pmnt at valuation date
# 			dum <- function(x) {zcb.ou.pv(sr, theta, phi, tstart=t, x)}
# 			disc <- vapply(mydates,1,FUN=dum)
# 			out <- sum(incl * disc * mycfs)
# 		};
# 	return(out)
# }
# 
# fcb$pv <- pv.ou.fcb(fcb, r0, theta, phi, t=currdate)
# fcb$pv
# 
# library(lifecontingencies)
# getirr <- function(p) {
# 	(presentValue(cashFlows=fcb$cfdf$cfs, timeIds=(fcb$cfdf$dates - fcb$issuedate),interestRates=p) - fcb$pv)^2
# }
# fcb$ytm <- nlm(f=getirr, p=.1)$estimate