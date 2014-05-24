# source("/Volumes/amy/LifeCon/R/Actuarial.R")
# 
# # Profit test example from Actuarial Mathematics for Life Contingent Risks,
# # section 11.2
# 
# .ones <- function(temp) {as.array(rep(1, length(temp)))}
# 
# InsFactors <- function(q, i, end) {
# 	# insurance factors
# 	# set end=0 for term, =1 for whole life or endowment insurance
# 	out <- BLFOR(q/(1+i), (1-q)/(1+i), end)
# 	return(out)
# }
# 
# IAFactors <- function(q, i, end) {
# 	# immediate annuity factors
# 	out <- BLFOR(.ones(q), (1-q)/(1+i), end)
# 	return(out)
# }
# 
# 
# Benefits <- list()
# Benefits$death <- function(x=0, t=0, ...) {10^5}
# Benefits
# 
# ReserveBasis <- list()
# ReserveBasis$interest <- function(t=0, ...) {0.04}
# ReserveBasis$qxt <- seq(0.011, 0.020, 0.001)
# ReserveBasis
# 
# # Reserve factors
# ResInsFactors <- InsFactors(q=ReserveBasis$qxt, i=ReserveBasis$interest(),
# 							end=0)
# ResInsFactors
# ResIAFactors <- IAFactors(q=ReserveBasis$qxt, i=ReserveBasis$interest(),
# 							end=0)
# ResIAFactors
# ResBenPrem <- Benefits$death()*ResInsFactors[1]/ResIAFactors[1]
# ResBenPrem # matches the book to two decimal places
# 
# ResPP <- ResInsFactors*Benefits$death() - ResBenPrem*ResIAFactors
# ResPP # matches the book again
# 
# ExperienceBasis <- list()
# ExperienceBasis$interest <- function(time=0, ...) {0.055}
# ExperienceBasis$InitExpense <- function(t=0, Premium=0, ...) {
# 	if(t==0) 400 + Premium*0.20 else 0
# }
# ExperienceBasis$RenewalExpense <- function(t, Premium=0, ...) {Premium*0.035}
# 
# ExperienceBasis$Expense <- function(time, Premium=0) {
# 	out <- rep(NA, length(time))
# 	for (j in 1:length(time)) {
# 		t <- time[j]
# 		if(j==1) {
# 			out[j] <- ExperienceBasis$InitExpense(t=t, Premium=Premium)
# 		}
# 		if(j==2) {
# 			out[j] <- 0
# 		}
# 		if(j>2.5) {
# 			out[j] <- ExperienceBasis$RenewalExpense(t=t, Premium=Premium)
# 		}
# 	}
# 	return(out)
# }
# ExperienceBasis$qx.tm1 <- function(x=0, time=0, ...) {
# 	out< rep(NA, length(time)-1)
# 	for (j in 1:length(out)) {
# 		t <- time[j]
# 		if(t==0) {
# 			out[j] <- NA
# 			} else {
# 				out[j] <- 0.010 + 0.001*(t-1)
# 		}
# 	}
# 	return(out)
# }
# 
# ModeledPoints <- list()
# ModeledPoints$n <- 1 # nominal number of policies
# ModeledPoints$AgeAtEntry <- 60
# ModeledPoints$type <- "TermLife"
# ModeledPoints$Term <- 10
# ModeledPoints$DeferralPeriod <- 0
# GrossPremPP <- 1500
# 
# time <- seq(0,10,1)
# time
# 
# cf.data <- data.frame(time)
# cf.data$Res.PPtm1 <- c(NA, ResPP)
# cf.data$Prem.PPtm1 <- c(NA, rep(GrossPremPP, length(cf.data$time)-1))
# cf.data$Expense.PPtm1 <- ExperienceBasis$Expense(time=cf.data$time, 
# 	Premium=GrossPremPP)
# cf.data$Interest.PPtm1.t <- (cf.data$Res.PPtm1 + cf.data$Prem.PPtm1 - cf.data$Expense.PPtm1) * ExperienceBasis$interest()
# cf.data$DeathBen.PPtm1.t <- c(NA, Benefits$death()*ExperienceBasis$qx.tm1(time=cf.data$time))
# cf.data$Res.PPtm1.t <- c(NA, ResPP[2:length(ResPP)]*(1 - ExperienceBasis$qx.tm1(time=cf.data$time)))
# cf.data

#######
# 2014-04-13
source("/Volumes/hd500/LifeCon/LifeCon/R/Actuarial.R")

# Profit test example from Actuarial Mathematics for Life Contingent Risks,
# section 11.2

.ones <- function(temp) {as.array(rep(1, length(temp)))}

InsFactors <- function(q, i, end) {
	# insurance factors
	# set end=0 for term, =1 for whole life or endowment insurance
	out <- BLFOR(q/(1+i), (1-q)/(1+i), end)
	return(out)
}

IAFactors <- function(q, i, end) {
	# immediate annuity factors
	out <- BLFOR(.ones(q), (1-q)/(1+i), end)
	return(out)
}

time <- seq(0,10,1)

Benefits <- list()
Benefits$death <- function(x=0, t=0, ...) {10^5}
Benefits

ReserveBasis <- list()
ReserveBasis$interest <- function(t=0, ...) {0.04}
ReserveBasis$qxt <- seq(0.011, 0.020, 0.001)
ReserveBasis

# Reserve factors
ResInsFactors <- InsFactors(q=ReserveBasis$qxt, i=ReserveBasis$interest(),
							end=0)
ResInsFactors
ResIAFactors <- IAFactors(q=ReserveBasis$qxt, i=ReserveBasis$interest(),
							end=0)
ResIAFactors
ResBenPrem <- Benefits$death()*ResInsFactors[1]/ResIAFactors[1]
ResBenPrem # matches the book to two decimal places

ResPPtm1 <- c(ResInsFactors*Benefits$death() - ResBenPrem*ResIAFactors,0)
ResPPtm1 # matches the book again
length(ResPPtm1)

gp <- 1500
Expense.init <- function(prem) {400 + 0.20*prem}
Expense.renew <- function(prem) {0.035*prem}
exp.qxt <- function(t) {0.01 + 0.001*t}
exp.int <- 0.055

navec <- rep(NA, length(time))
cf.data <- data.frame(time,
	ResPPtm1=navec,
	PremPPtm1=navec,
	InitExpensePP=navec,
	RenewExpensePPtm1=navec,
	InvIncPPtm1.t=navec,
	DeathBenPPtm1.t=navec,
	ResPPtm1.t=navec,
	PrPPtm1.t=navec
)
j=2
for (j in 1:length(cf.data$time)) {
	t <- cf.data$time[j]
	temp <- cf.data[which(cf.data$time==t),]
	if(t==0) {
		temp$InitExpensePP <- Expense.init(prem=gp)
		} else {
			temp$ResPPtm1 <- ResPPtm1[t]
			temp$PremPPtm1 <- gp
			temp$InitExpensePP <- 0
			ifelse(j==2,0,temp$RenewExpensePPtm1 <- Expense.renew(prem=gp))
			temp$InvIncPPtm1.t <- (
				temp$ResPPtm1 + 
				temp$PremPPtm1 -
				temp$RenewExpensePPtm1
				) * exp.int
			temp$DeathBenPPtm1.t <- Benefits$death()*exp.qxt(t-1)
			temp$ResPPtm1.t <- ResPPtm1[t+1]*(1 - exp.qxt(t-1))
			temp$PrPPtm1.t <- (
				temp$ResPPtm1 + 
				temp$PremPPtm1 -
				temp$RenewExpensePPtm1 +
				temp$InvIncPPtm1.t -
				temp$DeathBenPPtm1.t -
				temp$ResPPtm1.t
				)
		}
	cf.data[which(cf.data$time==t),] <- temp
}

cf.data

# 2014-05-24
df <- cf.data
PrPPtm1.t <- function(df) {
	times <- df$time[3:length(df$time)]
	temp0 <- numeric(length(times))
	dum <- function(my.t, my.df) {
		temp <- my.df[which(my.df$time==my.t),]
		out1 <- temp$ResPPtm1 + 
				temp$PremPPtm1 -
				temp$RenewExpensePPtm1 +
				temp$InvIncPPtm1.t -
				temp$DeathBenPPtm1.t -
				temp$ResPPtm1.t
		return(out1)
	}
	temp0 <- unlist(mapply(dum, my.t=times, MoreArgs=list(my.df=df)))
	out0 <- data.frame(times, Pr_t=temp0)
	return(out0)
}
PrPPtm1.t(cf.data)