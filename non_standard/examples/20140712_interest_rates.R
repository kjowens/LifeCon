# http://blog.revolutionanalytics.com/2014/06/quantitative-finance-applications-in-r-6-constructing-a-term-structure-of-interest-rates-using-r-par.html
# http://blog.revolutionanalytics.com/2014/07/quantitative-finance-applications-in-r-7-constructing-a-term-structure-of-interest-rates-using-r-par.html

require(lubridate)
require(xts)

# ad = anchor date, tz = time zone
# (see http://en.wikipedia.org/wiki/List_of_tz_database_time_zones)

##########################################################
# functions
##########################################################

.jtd <- function(x) {
	# "just the date"
	# Use substring(.) to get rid of "UTC"/time zone after the dates
	return(as.Date(substring(x, 1, 10)))
}

mdparseOne <- function(anchorDate, x) {
	# "md" = "market date"
	# used to parse one string in a vector that describes time associated with rate
	type <- substring(x,nchar(x),nchar(x))
	if( !(type %in% c("d","w","m","y")) ) {
		stop("The last character must be d, w, m, or y.")
	}
	out <- NULL
	value <- as.numeric(substring(x,1,nchar(x)-1))
	out <- switch(type,
				"d" = days(value),
				"w" = weeks(value),
				"m" = months(value),
				"y" = years(value)
				)
	out <- .jtd(anchorDate + out)
	return(out)
}

createMarketDates <- function(ad, Dates=NA) {
	# ad = anchor date
	# depends on the rates you are quoting
	# first date should be zero something to include the starting date
		if(is.na(Dates[1])) {
			Dates <- c("0d", "1d", "1w", "1m", "2m", "3m", "6m", "9m", "1y",
					   "2y", "3y", "5y", "7y", "10y", "15y", "20y", "25y", "30y")
		}
	Dates <- as.array(Dates)
	marketDates <- as.Date( apply(Dates, 1, FUN = function(x) mdparseOne(ad, x)))
	return(marketDates)
}

marketData2xts <- function(anchorDate,
						   marketRates,
						   Dates=NA) {
	# converts dates to an xts object
	marketDates <- createMarketDates(anchorDate, Dates)
	out <- NULL
	out <- as.xts(marketRates, order.by = marketDates)
	attributes(out)$anchorDate <- .jtd(anchorDate)
	return(out)
}

createEmptyTSXtsLub <- function(marketData.xts = NA, anchorDate = NA, plusYears) {
	# makes an empty term structure using xts and lubridate
	# anchorDate is a lubridate here:
	if( all( is.na(marketData.xts),is.na(anchorDate) ) ) {
		stop("An anchor date or a proper xts object with 'anchorDate' attribute must be specified.")
	}
	if(is.na(anchorDate)) {
		anchorDate <- attributes(marketData.xts)$anchorDate
	}
	endDate <- anchorDate + years(plusYears)
	numDays <- endDate - anchorDate  
	# We need to convert anchorDate to a standard R date to use
	# the "+ 0:numDays" operation.
	# Also, note that we need a total of numDays + 1 in order to capture both end points.
	xts.termStruct <- xts(rep(NA, numDays + 1), as.Date(anchorDate) + 0:numDays)
	return(xts.termStruct)
}

TSinterp <- function(x) {
	# choose the function to interpolate in the xts term structure
	switch(x,
		(function(TS) {na.spline(TS, method = "hyman")}),
		linear = (function(TS) {na.approx(TS)}))
}


MDxts2TS <- function(marketData.xts, plusYears=30, interpMeth = "spline") {
	# insert market data into an empty term structure
	# both are xts objects
	ad <- attributes(marketData.xts)$anchorDate
	mdates <- index(marketData.xts)
	myTS <- createEmptyTSXtsLub(marketData.xts, plusYears=plusYears)
	myTS[mdates] <- marketData.xts
	myTS <- TSinterp(interpMeth)(myTS)
	return(myTS)
}

# bringing it all together
createTermStructure <- function(anchorDate,
								marketRates,
								Dates=NA,
								plusYears=30,
								interpMeth = "spline"
								) {
	out <- NULL
	marketData.xts <- marketData2xts(anchorDate, marketRates, Dates)
	out <- MDxts2TS(marketData.xts, plusYears, interpMeth)
	attributes(out)$anchorDate <- .jtd(anchorDate)
	attributes(out)$origDates <- Dates
	return(out)
}

# day-counting functions

# Simple example of a day count function: Actual / 365 Fixed
# date1 and date2 are assumed to be lubridate dates, so that we can
# easily carry out the subtraction of two dates.
# all functions are always (earlier date, later date)


datechecker <- function(dt1, dt2) {
	# this caused an error, not really using this.
	if(dt1>dt2){ stop("all functions are always (earlier date, later date)")}
}

dayCountFcn_Act365Fixed <- function(date1, date2) {
	# datechecker(date1, date2)
    yearFraction <- as.numeric((date2 - date1)/365)
    return(yearFraction)  
}

dayCountFcn_Act360Fixed <- function(date1, date2) {
	# datechecker(date1, date2)
    yearFraction <- as.numeric((date2 - date1)/360)
    return(yearFraction)  
}

dayCountFcn_ActAct <- function(date1, date2) {
	# datechecker(date1, date2)
	y1 <- year(date1)
	y2 <- year(date2)
	ydf <- function(y) {ifelse(leap_year(y1),366,365)}
	if(y1==y2) {
		yearFraction <- (interval(date1, date2)/edays(1)) / ydf(y1)
	}
	if(y2-y1>0.5) {
		# sum proportions of each year
		dy1 <- (interval(date1, 						ymd(as.character(paste(year(date1),"12","31",sep="")))
						)/edays(1)
				) / ydf(date1)
		dy2 <- (interval(ymd(as.character(paste(year(date2)-1,"12","31",sep=""))),
						 date2
						)/edays(1)
				) / ydf(date2)
		yearFraction <- dy1 + dy2 + y2 - y1 - 1
	}
    return(yearFraction)  
}

dayCountFcn_30360 <- function(date1, date2) {
	# datechecker(date1, date2)
	yearFraction <- NULL
	dif.y <- year(date2) - year(date1)
	dif.m <- month(date2) - month(date1)
	dif.d <- day(date2) - day(date1)
	yearFraction <- (360*dif.y + 30*dif.m + dif.d) / (360)
	return(yearFraction)
}

chooseDCF <- function(type) {
	switch(type,
		"30360" = function(d1, d2) {dayCountFcn_30360(d1,d2)},
		"Act365Fixed" = function(d1, d2) {dayCountFcn_Act365Fixed(d1,d2)},
		"Act360Fixed" = function(d1, d2) {dayCountFcn_Act360Fixed(d1,d2)},
		"ActAct" = function(d1, d2) {dayCountFcn_ActAct(d1,d2)},
		)
}

DCF <- chooseDCF("30360") # default "day counter function"
# DCF(ad, b)
# DCF <- chooseDCF("Act365Fixed")
# DCF(ad, b)
# DCF <- chooseDCF("Act360Fixed")
# DCF(ad, b)
# DCF <- chooseDCF("ActAct")
# DCF(ad, b)

# note that we're assuming rates are continuously compounded

fwdDiscFact <- function(date1, date2, xtsMarketData, anchorDate=NA, 						DCFtype="30360") {
	# forward discount factor
	DCF <- chooseDCF(DCFtype)
	if(is.na(anchorDate[1])) {
		anchorDate <- as.Date(attributes(xtsMarketData)$anchorDate)
	}
    # Convert lubridate dates to base R dates in order to use as xts indices.
    xtsDate1 <- as.Date(date1)
    xtsDate2 <- as.Date(date2)
    if((xtsDate1 > xtsDate2) | xtsDate2 > max(index(xtsMarketData)) |
         xtsDate1 < min(index(xtsMarketData)))
         {
            stop("Error in date order or range")
         }
    # 1st, get the corresponding market zero rates from our
    # interpolated market rate curve:
    rate1 <- as.numeric(xtsMarketData[xtsDate1])      # R(0, T1)
    rate2 <- as.numeric(xtsMarketData[xtsDate2])      # R(0, T2)
    # P(0, T) = exp(-R(0, T) * (T - 0))   (A), with t = 0 <=> anchorDate
    discFactor1 <- exp(-rate1 * DCF(anchorDate, date1))
    discFactor2 <- exp(-rate2 * DCF(anchorDate, date2))
    # P(t, T) = P(0, T) / P(0, t)  (C), with t <=> date1 and T <=> date2
    fwdDF <- discFactor2/discFactor1
 return(fwdDF)
}


# Finally, we can then write a function to compute the forward interest rate:

# date1 and date2 are assumed to be lubridate dates here as well.
fwdIntRate <- function(date1, date2, xtsMarketData, anchorDate=NA, 						DCFtype="30360") {
	# forward continuously compounded interest rate
	DCF <- chooseDCF(DCFtype)
	if(is.na(anchorDate[1])) {
		anchorDate <- as.Date(attributes(xtsMarketData)$anchorDate)
	}
     if(date1 == date2) {
        fwdRate = 0.0 # the trivial case
     } else { 
           fwdDF <- fwdDiscFact(date1, date2,
                               xtsMarketData, DCFtype=DCFtype)
           # R(t, T) = -log(P(t, T)) / (T - t)    (B)
           fwdRate <- -log(fwdDF)/DCF(date1, date2)
            }
        return(fwdRate)
}


FCLFOR<-function(r,s,bv) {
	# Forward Cumulative Linear First Order Recurrence - for bootstrapping int. rates
	# Final boundary value is bv
	# The recurrence is x_n = r_n + s_n * x_{n-1}
	# Must be implemented in C
	# here's an example to show how it works:
	# FCLFOR(1:4, 5:8, 0)
	# 1+5*0  # =   1
	# 2+6*1  # =   8
	# 3+7*9  # =  66
	# 4+8*75 # = 604
	out <- NULL
	temp <- bv
	temp.sum <- 0
	dum <- function(x) {
		temp <<- x[1] + x[2]*temp.sum
		temp.sum <<- temp + temp.sum
		return(temp)
	}
	out <- as.array(apply(
							cbind(array(r,dim=length(s)),array(s))[1:length(s),],
							1,
							FUN=dum
						)[1:length(s)]
		);
	return(out)
}

mthlyYields2ccZCB <- function(TermStructure.xts, m = 2, term = 30, DCFtype="30360") {
	# returns a list of:
		# 1. Discount factors
		# 2. continuously compounded spot rates
	out <- list()
	DCF <- chooseDCF(DCFtype)
	h = 1/m # time between coupon payments
	d = 12/m # number of months between coupon payments
	n = term*m # total number of bonds we'll need to price, ignoring fractional bonds for now, and not including time zero
	d.times <- seq(0, n*d, d) # number of months
	mydates <- as.Date(sapply(d.times, FUN = function(x) .jtd(ad+months(x))))
	t.times <- sapply(mydates, FUN = function(x) DCF(ad, x)) # depends on DCF
	SelectedRates <- TermStructure.xts[mydates]
	# bootstrapping discount factors have a recursive function of the form
	# x(k) = r + s*S(k-1)
	# where S(i) is the sum of the first ith discount factors
	# a little complicated because we're including time zero, se we need extra 1
	r <- sapply(1:(n+1), FUN = function(x) (1 + h*SelectedRates[mydates[x]] 												)^{-1})
	temp.s <- sapply(1:(n+1), FUN = function(x) h*SelectedRates[mydates[x]])
	s <- -a.s*temp.b
	as.array(cbind(head(t.times), head(SelectedRates), head(a.s), head(b.s)))
	
	vt <- FCLFOR(r, s, 0) # discount factors to times
	
	vt.xts <- as.xts(vt, order.by = mydates)
	attributes(vt.xts)$anchorDate <- as.Date(.jtd(ad))
	out$DiscountFactors.xts <- vt.xts
	
	ccspot <- -log(vt)*m/c(1,1:n)
	ccspot.xts <- as.xts(ccspot, order.by = mydates)
	attributes(ccspot.xts)$anchorDate <- as.Date(.jtd(ad))
	out$ZCByield.xts <- ccspot.xts
	return(out)
}

# TODO: functions to convert one type of curve to another
#	bond equivalent <==> annual effective
#	continuously compound <==> annual effective


##########################################################
# example
##########################################################

ad <- ymd(20140514, tz = "US/Pacific")
# Convert percentage formats to decimal by multiplying by 0.01:
mktrates <- c(0.00, 0.08, 0.125, 0.15, 0.20, 0.255, 0.35, 0.55, 1.65,
              2.25, 2.85, 3.10,  3.35, 3.65, 3.95,  4.65, 5.15, 5.85) * 0.01
Dates <- c("0d", "1d", "1w", "1m", "2m", "3m", "6m", "9m", "1y",
		   "2y", "3y", "5y", "7y", "10y", "15y", "20y", "25y", "30y")

myts <- createTermStructure(ad, mktrates, Dates) # my term structure
mmyts <- head(myts, 10)
attributes(mmyts)

ad <- ymd(20140701, tz = "US/Pacific")
# Convert percentage formats to decimal by multiplying by 0.01:
mktrates <- c(0.00, 0.03, 0.02, 0.06, 0.11, 0.47, 0.90,
		 	  1.66, 2.17, 2.58, 3.13, 3.40) * 0.01
Dates <- c("0d", "1m", "3m", "6m", "1y", "2y", "3y",
	 	   "5y", "7y", "10y", "20y", "30y")
myts <- createTermStructure(anchorDate=ad, mktrates, Dates, interpMeth = "linear") # my term structure
plot(myts)
plot(mthlyYields2ccZCB(myts)$ZCByield.xts)
# unfortunately hyman spline doesn't like if you have rates that aren't monotonic
# but then, if you take the hyman flag out, it can dip in the future