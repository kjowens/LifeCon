# 2014-03-30
# Let's make an S3 class
# This is all experimental for a larger "DecrementRule" class?


Hazfactor.Makeham <- function(x, t, params, ...) {
	A <- params$A
	B <- params$B
	c <- params$c
	out <- A*t + B*(c^x)*(c^t - 1)
}
SK.mh <- function(x=50, h=1, len=10, type="mh", params=list(A=.001, B=.1, c=1.05)) {
	out <- rep(NA, len)
	if(type=="mh") {
		for (j in 1:len) {
			out[j] <- exp(-Hazfactor.Makeham(x=x, t=h*j, params))
		}
	}
	return(out)
}

myparams <- list(A=.001, B=.1, c=1.05)
myp <- SK.mh(x=50, h=1, len=100, type="mh", params=myparams)
myp
# plot(myp)


ptx2OYqs <- function(SK) {
	out <- rep(NA, length(SK))
	for (j in 1:length(SK)) {
		if(j==1) {
			out[j] <- 1-SK[j]
		}
		else {
			out[j] <- 1 - SK[j]/SK[j-1]
		}
	}
	return(out)
}

ptx2OYqs(myp)