# 2014-07-06, KJO: SOA table retrieval functions, and functions to use the output

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
