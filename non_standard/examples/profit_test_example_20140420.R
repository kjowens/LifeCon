#######
# 2014-04-13
library(rbenchmark)

# fun <- function() {
source("/Volumes/amy/LifeCon/R/Actuarial.R")

# Profit test example from Actuarial Mathematics for Life Contingent Risks,
# section 11.2

# Some useful functions
.ones <- function(temp) {as.array(rep(1, length(temp)))}

.DiscountFact <- function(i, m) {
	# returns a discount factor based on an anual effective interest rate
	if(m==0) {stop("The frequency, 'm' can't be zero")} else {
		out <- NULL
		out <- {1+i}^-{1/m}
	}
	return(out)
}

BenFactors <- function(OYq, i, PB=PolBasis) {
	out <- NULL
	end <- PB$SurvFactor
	freq <- PB$BenFreq
	q <- qOY2qmthly(q=OYq, m=freq, FAA=PB$FAA)
	v <- .DiscountFact(i,m=freq) * .ones(freq*PB$TermDur)
	out <- BLFOR(q*v, (1-q)*v, end)
	return(out)
}

PremFactors <- function(OYq, i, PB=PolBasis) {
	out <- NULL
	end <- 0 # not reasonable for them to pay a premium at the end
	freq <- PB$PremFreq
	q <- qOY2qmthly(q=OYq, m=freq, FAA=PB$FAA)
	v <- .DiscountFact(i,m=freq) * .ones(freq*PB$TermDur)
	out <- BLFOR(.ones(q), (1-q)*v, end)
	return(out)
}

.GetPolDur <- function(Date, IssueDate) {
	out <- NULL
	out <- Date - IssueDate
	return(out)
}

# Projection scope
ProjStartYear <- 2015
# I'm going to use begining of the month as the standard
# i.e., Jan 2015 means 1/1/2015, or 2015.00; 12/31/2014 in some other programs
ProjStartMonth <- 1
ProjStartDate <- ProjStartYear + (ProjStartMonth - 1)/12 # as float in years

ProjEndYear <- 2025
ProjEndMonth <- 1
ProjEndDate <- ProjEndYear + (ProjEndMonth - 1)/12 # as float in years

ProjDur <- ProjEndDate - ProjStartDate # Projection Duration
# ProjDur

# Projection frequency
#  1 <- annual
#  2 <- semi-annual
#  3 <- every four months
#  4 <- quarterly
# 12 <- monthly
ProjFreq <- 1

ProjDates <- seq(ProjStartDate,ProjEndDate,ProjFreq)
ProjTimes <- seq(0,ProjDur,ProjFreq)

# Here's some policy specific stuff
PolBasis <- list()
PolBasis$IssueYear <- 2015
PolBasis$IssueMonth <- 1 # same date conventions as projection
PolBasis$IssueDate <- PolBasis$IssueYear + (PolBasis$IssueMonth - 1)/12
PolBasis$IssueAge <- 60
PolBasis$TermDur <- 10 # the term policy only goes for 10 years
PolBasis$BenFreq <- 1
PolBasis$PremFreq <- 1
PolBasis$FAA <- 3
PolBasis$SurvFactor <- 0 # 0 for term, 1 for WL or endowment

Benefits <- list()
Benefits$death <- function(x=0, t=0, ...) {10^5}
# Benefits

# Reserve Basis
ResBasis <- list()
ResBasis$interest <- function(t=0, ...) {0.04} # annual effective
ResBasis$qxt <- seq(0.011, 0.020, 0.001) # one year qx's
# ResBasis

# Reserve factors

# ResBenPrem # matches the book to two decimal places

ResPP <- function(x=0, date=2015, PSD=ProjStartDate, PB=PolBasis,
	RB=ResBasis) {
		# TODO: allow for different premium and benefit dates
	PolDur <- .GetPolDur(Date=date,IssueDate=PB$IssueDate)
	if({PolDur<0} || {PolDur>PB$TermDur}) {
		out <- 0
		} else {
			# this part will need to be more robust for mthly, break it out
			# into it's own function
			ResBenFactors <- BenFactors(OYq=RB$qxt,
				i=RB$interest(),
				PB=PolBasis)
			ResPremFactors <- PremFactors(OYq=RB$qxt,
				i=RB$interest(),
				PB=PolBasis)
			ResBenPrem <- Benefits$death()*ResBenFactors[1]/ResPremFactors[1]
			ResVec <- c(ResBenFactors*Benefits$death() -
				ResBenPrem*ResPremFactors,0)
				length(ResVec)
			ResTimes <- seq(0,PB$TermDur,PB$BenFreq)
			ResDates <- PB$IssueDate + ResTimes
			length(ResDates)
			tempdf <- data.frame(ResDates, ResVec)
			out <- tempdf[which(tempdf$ResDates==date),]$ResVec # TODO needs to be change for m-thly, when date not on reserve calc date
		}	
	return(out)
}

gp <- 1500
Expense.init <- function(prem) {400 + 0.20*prem}
Expense.renew <- function(prem) {0.035*prem}
exp.qxt <- function(t) {0.01 + 0.001*t}
exp.int <- 0.055

navec <- rep(NA, length(ProjDates))
cf.data <- data.frame(ProjDates,
	ProjTimes,
	ResPPtm1=navec,
	PremPPtm1=navec,
	InitExpensePP=navec,
	RenewExpensePPtm1=navec,
	InvIncPPtm1.t=navec,
	DeathBenPPtm1.t=navec,
	ResPPtm1.t=navec,
	PrPPtm1.t=navec
)
#j=11 # for debuging
for (j in 1:dim(cf.data)[1]) {
	projdate <- cf.data$ProjDates[j]
	t <- projdate - ProjStartDate
	prev.t <- t - ProjFreq
	prev.projdate <- projdate - ProjFreq
	temp <- cf.data[j,]
	if(t==0) {
		temp$InitExpensePP <- Expense.init(prem=gp)
		temp$PrPPtm1.t <- - Expense.init(prem=gp)
		} else {
			temp$ResPPtm1 <- ResPP(date=prev.projdate)
			temp$PremPPtm1 <- gp
			temp$InitExpensePP <- 0
			temp$RenewExpensePPtm1 <- ifelse(j==2,0,Expense.renew(prem=gp))
			temp$InvIncPPtm1.t <- (
				temp$ResPPtm1 + 
				temp$PremPPtm1 -
				temp$RenewExpensePPtm1
				) * exp.int
			temp$DeathBenPPtm1.t <- Benefits$death()*exp.qxt(t-1)
			temp$ResPPtm1.t <- ResPP(date=projdate)*(1 - exp.qxt(t-1))
			temp$PrPPtm1.t <- (
				temp$ResPPtm1 + 
				temp$PremPPtm1 -
				temp$RenewExpensePPtm1 +
				temp$InvIncPPtm1.t -
				temp$DeathBenPPtm1.t -
				temp$ResPPtm1.t
				)
		}
	cf.data[j,] <- temp
}

# return(cf.data)
cf.data
# } # to make the whole thing a function for benchmarking

# benchmark(fun, replications=10)

# myfun <- function() {a <- NULL}
# benchmark(myfun(), replications=10^4)
# so, assigning the output name to NULL is inefficient, but for now I'll keep it

# https://stackoverflow.com/questions/4357101/promise-already-under-evaluation-recursive-default-argument-reference-or-earlie
# moral of the story: don't name arguments the default input