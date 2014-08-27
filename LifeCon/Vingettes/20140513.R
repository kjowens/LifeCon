pFAA.0 <- function(q, t) {ifelse(t>0,q,0)}
pFAA.1 <- function(q, t) {ifelse(t<1,0,q)}
pFAA.2 <- function(q, t) {1-t*q}
pFAA.3 <- function(q, t) {(1-q)^t}
pFAA.4 <- function(q, t) {(1-t + t/(1-q))^-1}

pFAA.prep <- function(q, t, FAA) {
	out <- numeric(1)
	if(any(q<0,q>1,t<0,t>1, !(FAA %in% 0:4))) {
		stop("arguments outside of their domain")
		} else {
			out <- switch(FAA+1,
					pFAA.0(q, t),
					pFAA.1(q, t),
					pFAA.2(q, t),
					pFAA.3(q, t),
					pFAA.4(q, t)
				)
		}
		return(out)
}

pFAA <- function(q, t, FAA) {
	mapply(pFAA.prep, q, t, FAA)
}

library(RUnit)

test.pFAA <- function() {
	checkException(pFAA(-1, .3, 2))
	checkException(pFAA(2, .3, 2))
	checkException(pFAA(.4, -1, 2))
	checkException(pFAA(.4, 2, 2))
	checkException(pFAA(.4, .3, -1))
	checkException(pFAA(.4, .3, 5))
	checkEquals(pFAA(.4, .3, 0), .4)
	checkEquals(pFAA(.4, 0, 0), 0)
	checkEquals(pFAA(.4, .3, 1), 0)
	checkEquals(pFAA(.4, 1, 0), .4)
	checkEquals(pFAA(.4, .3, 2), .88)
	checkEquals(pFAA(.4, .3, 3), .6^.3)
	checkEquals(pFAA(.4, .3, 4), (.7 + .3/.6)^-1)
}


