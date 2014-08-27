# an example with short rate models



## translating ASM parameterization to sde package params
# ou.a <- .2
# ou.b <- .1
# ou.vol <- .1
# theta1 <- ou.b*ou.a
# theta2 <- ou.a
# theta3 <- ou.vol
# theta <- c(theta1, theta2, theta3)
# phi <- .15




myparam <- ASMparam2SDE(a=.2, b=.1, sigma=.12, phi=.25)
myparam
ou.rbar(myparam)

sr0 <- 0.06 # setting a short rate at time zero
zcb.ou(param=myparam, sr=sr0, tstart=0, tend=5)
# can validate from ASM MFE example 26L, pg 534

# test <- function(time) {zcb.ou(myparam, sr=.06, tstart=0, tend=time)$pv}
# test(5)



# fixed coupon bond
# "dates" are just years as floating point number
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


cur_cfdf <- remaining_cfs(currdate, fcb$cfdf)
cur_cfdf

myparam <- ASMparam2SDE(a=.5, b=.1, sigma=.12, phi=.25)
time_step = 0.5
# my_times = seq(2015, 2016, .5)
# length(my_times)
# my_rates <- rcOU(n=length(my_times), Dt=time_step, x0=sr0, theta=myparam$theta)
# plot(my_rates)

# sum of pv's method

cur_cfdf$zcbpv <- mapply(zcbpv, cur_cfdf$times, MoreArgs=list(param=myparam, sr=sr0))
cur_cfdf
cur_cfdf$pvcfs <- cur_cfdf$cfs * cur_cfdf$zcbpv
cur_cfdf
sum(cur_cfdf$pvcfs)

price_bond_sum_pv <- function(l_bond, cur_date, sr, param) {
  if(cur_date<l_bond$matdate) {
    out <- numeric(0)
    .sr <- sr
    .param <- param
    cur_cfdf <- remaining_cfs(cur_date, l_bond$cfdf)
    cur_cfdf$zcbpv <- mapply(zcbpv, cur_cfdf$times, MoreArgs=list(param=.param, sr=.sr))
    cur_cfdf$pvcfs <- cur_cfdf$cfs * cur_cfdf$zcbpv
    out <- sum(cur_cfdf$pvcfs)
  } else {
    out = 0
  }
  return(out)
}
price_bond_sum_pv(l_bond=fcb, cur_date=2015, sr=sr0, param=myparam)

book_values <- mapply(price_bond_sum_pv, my_times, MoreArgs=list(l_bond=fcb,param=myparam, sr=sr0))
length(book_values)
length(cur_cfdf$dates)
market_values <- mapply(price_bond_sum_pv, my_times, my_rates, MoreArgs=list(l_bond=fcb,param=myparam))
unrl_gl <- market_values - book_values

plot(book_values)
plot(market_values)
plot(unrl_gl)

# recursive method
# with the Vasicek model, only T-t matters, not specific values of T or t
# zcb_fw_pv <- function(date, step, param, sr) {zcb.ou(param, sr, tstart=date, tend=date+step)$pv}
# zcb_fw_pv(0, 1, param=myparam, sr=sr0)
# zcbpv(1, param=myparam, sr=sr0)

# load package w/o installing
library(devtools)
load_all('/Volumes/amy/LifeCon/LifeCon')

time_step
cur_cfdf
# # zcb_fw_pv <- zcbpv(time_step, param=myparam, sr=sr0)
# # zcb_fw_pv
# temp <- BLFOR(cur_cfdf$cfs, .ones(cur_cfdf$cfs)*zcb_fw_pv, 0)
# temp
# length(temp)
# cur_cfdf$temp <- temp
# cur_cfdf
# cur_cfdf$bv <- book_values[2:21]
# book_values

# a = cur_cfdf$temp[19]
# a
# a*zcb_fw_pv + 2
# sum(cur_cfdf$pvcfs[18:20])/cur_cfdf$zcbpv[18]

# cur_cfdf$zcbpv
# b <- cur_cfdf$zcbpv/c(1,cur_cfdf$zcbpv[1:(length(cur_cfdf$zcbpv)-1)])
# b
# length(b)

len <- length(cur_cfdf$cfs)
cur_cfdf$zcb_fw_pv <- cur_cfdf$zcbpv/c(1,cur_cfdf$zcbpv[1:(len-1)])
cur_cfdf
((cur_cfdf$zcbpv[2]/cur_cfdf$zcbpv[1])*102+2)*cur_cfdf$zcbpv[1]
price_bond_sum_pv(l_bond=fcb, cur_date=2015, sr=sr0, param=myparam)
# ok, that get's the same answer
# now I think I don't want to have the cash flow at valuation date included in the price
# we'll put that in "cash"

# , time_step=NA
price_bond_recursive <- function(l_bond, cur_date, sr, param, only_first=TRUE) {
  if(cur_date<l_bond$matdate) {
    out <- numeric(0)
    .sr <- sr
    .param <- param
    cur_cfdf <- remaining_cfs(cur_date, l_bond$cfdf)
    cur_cfdf$zcbpv <- mapply(zcbpv, cur_cfdf$times, MoreArgs=list(param=.param, sr=.sr))
    len <- length(cur_cfdf$cfs)  
    if(len>1) {
      cur_cfdf$zcb_fw_pv <- cur_cfdf$zcbpv/c(1,cur_cfdf$zcbpv[1:(len-1)])
      temp_r <- c(0,cur_cfdf$cfs[1:(len-1)])
      temp_s <- cur_cfdf$zcb_fw_pv
      temp_b <- cur_cfdf$cfs[len]
      # x(t) = r(t) + s(t)*x(t+1)
      out <- BLFOR(temp_r, temp_s, temp_b)
    } else {
      out <- sum(cur_cfdf$zcbpv * cur_cfdf$cfs)
    }
    if(only_first) {
      out <- out[1]
    }
  } else {
    out = 0
  }
  return(out)
}

mydate = 2015
price_bond_sum_pv(l_bond=fcb, cur_date=mydate, sr=sr0, param=myparam)
price_bond_recursive(l_bond=fcb, cur_date=mydate, sr=sr0, param=myparam)

sum_pv <- function(x) {price_bond_sum_pv(l_bond=fcb, cur_date=x, sr=sr0, param=myparam)}
recur <- function(x) {price_bond_recursive(l_bond=fcb, cur_date=x, sr=sr0, param=myparam)}

library(microbenchmark)
microbenchmark(sum_pv(2015), recur(2015))
# recursive method takes longer, but coded to get zcb's first, so that makes sense
# this is because of the way the forward discount factors must be calculated in the model




# CIR model
myparam <- ASMparam2SDE(a=.5, b=.1, sigma=.12, phi=.25)
cir.gamma(.param=myparam)
cir.rbar(.param=myparam)
cir.B(.param=myparam, tstart=0, tend=5)
cir.A(.param=myparam, tstart=0, tend=5)
zcb.cir.pv(sr=sr0, .param=myparam, tstart=0, tend=5)




# finished







help("mapply")
