# 2014-08-03
# setting up some basic insurance and annuity functions

get_qs <- function(age, table) {
	# Returns vector of mortality rates
	# assumes age is called "x" and mortality rates "qx"
	age <- floor(age)
	qs <- unlist(table[which(table$x > age - 0.5),]$qx)
	return(qs)
}

ia_0 <- function(q, v, pmt=1) {
	BLFOR(.ones(q)*pmt, (1-q)*v, 0)
}

wl_0 <- function(q, v) {
	BLFOR(q*v, (1-q)*v, 1)
}

term_0 <- function(q, v, term) {
	q_term <- min(length(q),term)
	q <- q[1:q_term]
	v_term <- min(length(v),term)
	v <- v[1:v_term]
	out <- BLFOR(v*q, (1-q)*v, 0)
	return(out)
}

endow_0 <- function(q, v, term, db=1, sb=1) {
	q_term <- min(length(q),term)
	q <- q[1:q_term]
	v_term <- min(length(v),term)
	v <- v[1:v_term]
	db_term <- min(length(db),term)
	db <- db[1:db_term]
	out <- BLFOR(v*q*db, (1-q)*v, sb)
	return(out)
}


# immediate_annuity <- function(age, table, discount_factors) {
#
# }

##############################
# testing
##############################

# https://stackoverflow.com/questions/5339796/loading-an-r-package-from-a-custom-directory
library(devtools)

# load package w/o installing
load_all('/Volumes/amy/LifeCon/LifeCon')

# or invoke 'R CMD INSTALL'
# install('/some/package/diR')

# rcpp_hello_world()

tbl <- SOAtbl2df.agg(15006)

# tbl
head(tbl)
tail(tbl)

get_qs(97, tbl)
q = c(rep(.3,3),1)
v = .9


q
v
ia_0(q, v)

q
v
wl_0(q, v)

q
v
term_0(q, v, 2)
term_0(q, v, 10)

q
v
endow_0(q, v, 2)
endow_0(q, v, 10)
wl_0(q,v)
