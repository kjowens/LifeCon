source("/Volumes/hd500/LifeCon/LifeCon/R/Actuarial.R")

SOAtbl2df.agg <- function(tblnumb, param=list(must.die=FALSE)) {
	# retrieves aggregate table from SOA website, then converts to data frame
	# age column is called x, one year death probabilities called qx
	require(XML)
	out <- NULL
	url <- paste("http://mort.soa.org/data/t",tblnumb,".xml",sep="")
	tempxml <- xmlParse(url)
	lhi <- xmlToList(tempxml)
	lhi2 <- lhi[2]$Table$Values$Axis
	dum2 <- function(.obj, i) {
		out2 <- list()
		temp1 <- .obj[i]$Y
		out2$x  <- as.numeric(temp1$.attrs)
		out2$qx <- as.numeric(temp1$text)
		return(out2)
	}
	indecies <- 1:length(lhi2)
	myMorArgs <- list(.obj=lhi2)
	out <- data.frame(t(mapply(dum2, i=indecies, MoreArgs=myMorArgs)))
	if(param$must.die==TRUE){ # sets the last qx equal to 1
		if(out[length(out[,1]),2]!=1){
			out[length(out[,1]),2] <- 1
		}
	}
	cust.att.agg <- function(tbl, ...) {
		attributes(tbl)$row.names <- paste(tbl$x)
		ltemp <- list()
		ltemp$age.min <- min(unlist(tbl$x))
		ltemp$age.max <- max(unlist(tbl$x))
		ltemp$omega <- min(unlist(tbl[which(tbl$qx==1),1]))
		ltemp$ContentClassification <- lhi$ContentClassification
		attributes(tbl) <- append(attributes(tbl), ltemp)
	}
	attributes(out) <- cust.att.agg(out)
	if(max(unlist(out$qx))>1){
		warning("at least one age has a qx greater than 1")
	}
	return(out)
}

tbl <- SOAtbl2df.agg(1585)
tbl

# tbl["99",2] <- .1
# temp <- min(unlist(tbl[which(tbl$qx==1),1]))
# temp # if no entry is 1, omega is inf
attributes(tbl)

get.qs.agg <- function(obj, my.from, my.to=my.from, my.by=1, param=list(too.young="error",too.old="error")) {
	# return a vector of death probabilities from an aggregate mortality table
	# from the SOA
	# TODO: too old and too young error handling
	# options should be cf, 0, 1
	ages <- seq(my.from, my.to, my.by)
	out <- numeric(length(ages))
	dum <- function(age, .obj) {
		.obj[which(.obj$x==age),2]
	}
	out <- unlist(mapply(dum, age=ages, MoreArgs=list(.obj=obj)))
	return(out)
}

get.xs.agg <- function(obj, my.from, my.to=my.from, my.by=1, param=list(too.young="error",too.old="error")) {
	# return a vector of death probabilities from an aggregate mortality table
	# from the SOA
	# TODO: too old and too young error handling
	# options should be cf, 0, 1
	ages <- seq(my.from, my.to, my.by)
	out <- numeric(length(ages))
	dum <- function(age, .obj) {
		.obj[which(.obj$x==age),1] # only change from get.qs.agg here
	}
	out <- unlist(mapply(dum, age=ages, MoreArgs=list(.obj=obj)))
	return(out)
}


get.qs.agg(tbl, 27, 37)




# eval(parse(text="tbl")) # this is how you evaluate code as a string
.evaltxt <- function(txt) {
	# evaluates arbitrary code as text, could be dangerous
	# useful when you need to iterate over a list of strings
	if(is.character(txt)){
		eval(parse(text=txt))
	}
}
iss.date <- 2015
iss.age <- 30
pol.mort <- "tbl"
pol.omega <- .evaltxt(paste("attributes(",pol.mort,")$omega",sep=""))
pol.term <- 10
pol.term <- ifelse(is.na(pol.term),max(0,pol.omega-iss.age),pol.term)
pol.ben.freq <- 0 # pays benefits continuously
pol.prem.freq <- 2 # premium paid semi-anually
cur.pol.vol <- 1 # policy volume, number of policies in policy record
# cur b/c we need to evaluate the policy volume at a point in time
# all policies could have lapsed or died at a previous time
pol.vol.prec <- 1e-5 # policy volume precision, if less than this consider it gone






# want to make a function to calculate reserve based on policy data and the current date
# must be flexible enough to calculate "reserve"
# 	before issue
# 	at issue
# 	between premium payments
# 	at payment of benefit, but not payment of premium
# 	at payment of premium but not payment of benefit
# 	at benefit and premium payment date
# 	after policy term has ended, but insured still alive
# 	when insured must be dead due to table, but policy term not ended

# dates expressed as floating point numbers, units in years

cur.date <- 2016 # current date is a property of the current state

cur.iss.elapse <- (cur.date - iss.date)
cur.age <- iss.age + cur.iss.elapse

inforce <- all(
	cur.date<iss.date+pol.term,
	cur.date>iss.date,
	cur.pol.vol-pol.vol.prec>0,
	cur.age>0,
	cur.age<pol.omega,
	iss.age<pol.omega)
inforce

if(inforce){
	cur.term <- pol.term - cur.iss.elapse
	} else{
		cur.term <- 0
}
cur.term

# need to be careful when at or around integer, just need to do some testing
get.qs.agg(.evaltxt(pol.mort), floor(cur.age), ceiling(cur.age+cur.term))
get.xs.agg(.evaltxt(pol.mort), floor(cur.age), ceiling(cur.age+cur.term))

# get.qs.agg(.evaltxt(pol.mort), floor(cur.age), ceiling(cur.age+Inf))
# so, it's a problem if it's infinite, maybe just need to be strict about making a table; any assumptions about too young or too old need to be address when making the table
