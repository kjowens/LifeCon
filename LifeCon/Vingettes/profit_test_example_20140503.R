#######
# 2014-04-13
library(rbenchmark)

# fun <- function() {
source("/Volumes/amy/LifeCon/LifeCon/R/Actuarial.R")

# Profit test example from Actuarial Mathematics for Life Contingent Risks,
# section 11.2


df.ProjectionParameters <-  read.csv("/Volumes/amy/LifeCon/non_standard/examples/ProjectionParameters.csv")
# Projection Parameters
Pjparam <- setProjParams(df)


mpf <- read.csv("/Volumes/amy/LifeCon/non_standard/examples/termMPF.csv")
setupMPF <- function(mpf) {
	out <- mpf
	attach(mpf)
	out$IssueDate <- .YM2date(IssueYear,IssueMonth)
	detach(mpf)
	return(out)
}
mpf <- setupMPF(mpf)
attach(mpf[1,])
# IssueDate

# speficic to a person: issue date, issue age, policy term, death benefit, premium, DB and prem frequency, mortality table
# specific to a policy type: suvival factor (0 or 1), 

# a <- 1
# get("a")

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

CurDate <- 2016

ResPP <- function(.PjParam=Pjparam, mp, CurDate, x=0, date=2015, PSD=ProjStartDate, PB=PolBasis,
	RB=ResBasis) {
	attach(.PjParam)
	attach(mp)
		# TODO: allow for different premium and benefit dates
	PolDur <- .GetPolDur(Date=CurDate,IssueDate)
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
	dettach(.PjParam)
	dettach(mp)
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