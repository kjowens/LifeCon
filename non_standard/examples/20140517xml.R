

SOAtbl2Matrix.agg <- function(tblnumb) {
	require(XML)
	out <- NULL
	url <- paste("http://mort.soa.org/data/t",tblnumb,".xml",sep="")
	tempxml <- xmlParse(url)
	lhi <- xmlToList(tempxml)
	lhi2 <- lhi[2]$Table$Values$Axis
	dum2 <- function(.obj, i) {
		out2 <- list()
		temp1 <- .obj[i]$Y
		out2$x <- as.numeric(temp1$.attrs)
		out2$q <- as.numeric(temp1$text)
		return(out2)
	}
	indecies <- 1:length(lhi2)
	myMorArgs <- list(.obj=lhi2)
	out <- t(mapply(dum2, i=indecies, MoreArgs=myMorArgs))
	return(out)
}

# SOAtbl2Matrix.agg(1585) <- 

# require(RUnit)
# test.SOAtbl2Matrix.agg <- function() {
# 	checkException(SOAtbl2Matrix.agg(-2))
# 	checkEquals(SOAtbl2Matrix.agg(1585)[73,2],1)
# }
# test.SOAtbl2Matrix.agg()

# here's a select and ultimate table
# url <- paste("http://mort.soa.org/data/t",2013,".xml",sep="")
# tempxml <- xmlParse(url)
# lhi <- xmlToList(tempxml)
# length(lhi)
# length(lhi[2]$Table$Values[2])
# names(lhi[2]$Table$Values)
# lhi[2]$Table$MetaData # this is the select table
# duration of 1 means the first year, not the second
# so, duration 1 means they are at age [x]+0

# lhi[3]$Table$MetaData # 3 is the ultimate table
# 
# lhi[2]$Table$Values[1] # age zero, 25 select years; the whole record
# lhi[2]$Table$Values[1]$Axis$Axis[1] # the first select period, q, then period number
# lhi[2]$Table$Values[1]$Axis$.attrs # the starting select age, [x]
# length(lhi[2]$Table$Values) # ages 0 to 99
# length(lhi[2]$Table$Values[1]$Axis$Axis) # duration 1 to 25
# at the begining of period 1 [0] is age zero, and at the end age [0]+1 = 1
# at the end of 25 periods [0] is age 25, so the age at the first ultimate period
# is 25


SOAtbl2Matrix.select <- function(tblnumb) {
	require(XML)
	out <- NULL
	url <- paste("http://mort.soa.org/data/t",tblnumb,".xml",sep="")
	tempxml <- xmlParse(url)
	lhi <- xmlToList(tempxml)
	lhi2 <- lhi[2]$Table$Values # select table
	lhi3 <- lhi[3]$Table$Values # ultimate table
	age.indecies <- 1:length(lhi[2]$Table$Values)
	select.periods <- length(lhi[2]$Table$Values[1]$Axis$Axis)
	select.indecies <- 1:select.periods
	myMorArgs <- list(.obj.s=lhi2, .obj.u=lhi3)
	
	dum0 <- function(dur.i, age.i, .obj.s) {
		# extracts q frome age/duration intersection
		out0 <- numeric(1)
		temp0 <- .obj.s[age.i]$Axis$Axis[dur.i]
		out0 <- as.numeric(temp0$Y$text)
		return(out0)
	}

	dum1 <- function(.obj.s, .obj.u, age.i) {
		# compiles row of select/ultimate table
		out1 <- numeric(select.periods+3)
		# 1 for initial age, 1 for ultimate q, 1 for ultimate age
		out1[1] <- as.numeric(.obj.s[age.i]$Axis$.attrs) # the initial age, [x]
		out1[2:(select.periods+1)] <- mapply(dum0, 
			dur.i=select.indecies, 
			MoreArgs=list(age.i=age.i, .obj.s=lhi2)
			)
		out1[(select.periods+2):length(out1)]
		out1[-(1:(select.periods+1))] <- dum2(.obj.u, age.i)
		return(out1)
	}

	dum2 <- function(.obj.u, age.i) {
		# extract the q and ultimate age for given age index
		out2 <- numeric(2)
		temp2 <- .obj.u$Axis[age.i]
		out2[1] <- as.numeric(temp2$Y$text)
		out2[2] <- as.numeric(temp2$Y$.attrs)
		return(out2)
	}
	
	out <- t(mapply(dum1, age.i=age.indecies, MoreArgs=myMorArgs))
	col.names <- c("[x]", 
		paste("q([x]+",select.indecies-1,")",sep=""),
		paste("q(x+",select.periods,")",sep=""), 
		paste("x+",select.periods,sep="")
		)
	init.select.ages <- paste("[",out[,1],"]",sep="")
	dimnames(out) <- list(init.select.ages, col.names)
	return(out)
}
# SOAtbl2Matrix.select(2013)