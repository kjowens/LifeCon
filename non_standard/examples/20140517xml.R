

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

# SOAtbl2Matrix.agg(1585)

# require(RUnit)
# test.SOAtbl2Matrix.agg <- function() {
# 	checkException(SOAtbl2Matrix.agg(-2))
# 	checkEquals(SOAtbl2Matrix.agg(1585)[73,2],1)
# }
# test.SOAtbl2Matrix.agg()