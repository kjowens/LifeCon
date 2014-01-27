# This example uses Kolmogorov's forward equation to numerically estimate transition probabilities for non-homogonous Markov chains.


.ChangeFrom.k <- function( i, j, mus, ps, k )
{
	# Inputs:
		# i: starting state
		# j: ending state
		# mus: matrix of hazard rates as a function of time
		# ps: matrix of state probabilities from the last iteration
		# k: the state under consideration
	# Output:
		# The change due to state k, k not equal to j
	out <- NULL
	out <- ps[i,k]*mus[k,j] - ps[i,j]*mus[j,k]
	return(out)
}

pijth <- function( i, j, mus, ps, h)
{
	# Probability state transition from i to j at time t+h given prob for time t
	# Inputs:
		# i: starting state
		# j: ending state
		# mus: matrix of hazard rates as a function of time
		# ps: matrix of state probabilities from the last iteration
		# h: the length of the period
	# Output:
		# probability that one starting in state i will be in state j at t+h
	out <- NULL
	pijt <- ps[i,j]
	changes <- rep(0, times=length( mus[1,]) - 1 )
	for (k in 1:length( mus[1,]) ) {
		if (k!=j) {
			changes[k] <- .ChangeFrom.k( i=i, j=j, mus=mus, ps=ps, k=k )
		}
	}
	out <- pijt + h*sum(changes)
	return(out)
}


KFE.pijt <- function( horizon=1, h=0.2, population.start, ForcesofMort, statesNames=NULL  )
{
	# Kolmogorov's forward equation, as a single formula
	out <- NULL
	p.start <- population.start
	TotalPopulation <- sum(p.start)
	mus <- ForcesofMort
	horizon <- horizon
	h <- h
	n <- horizon/h + 1 # + 1 for time zero
	ts <- seq(from=0, to=horizon, by=h)
	# making the output objet and filling with zeroes
	ps <- array(data=rep(0, times=(length(p.start)*n)),
				dim=c(n, length(p.start[1,]), length(p.start[,1])),
				dimnames=list(as.character(ts), statesNames, statesNames)
	)
	
	for (period in 1:length( ps[,1,1] )) {
		if (period==1) {
			ps[period,,] <- p.start
		}
		else{
			t <- as.numeric(names(ps[,1,1][period-1])) # the previous time period
			mus.t <- mus(t)
			pijs.t <- ps[period-1,,]
			for (i in 1:length( pijs.t[,1] )) {
				for (j in 1:length( pijs.t[i,] )) {
					ps[period,i,j] <- pijth( i=i, j=j, mus=mus.t, ps=pijs.t, h=h)
				}
		}
		}
	}
	out <- ps
	return(out)
}

# Define force of mortality functions
# States:
#	1 = Healthy
#	2 = Disabled
#	3 = Dead

statesNames <- c("Healthy", "Disabled", "Dead")

mu12 <- function( t )
{
	0.03 + 0.002*t
}

mu13 <- function( t )
{
	0.005 + 0.001*t
}

mu21 <- function( t )
{
	0.01*exp(-0.5*t)
}

mu23 <- function( t )
{
	0.05 + 0.003*t
}

mus <- function( t )
{
	out <- matrix(
			c(0, mu12(t), mu13(t),
			  mu21(t), 0, mu23(t),
			  0, 0, 0
			),
			nrow=3, ncol=3, byrow=TRUE, dimnames=list(statesNames, statesNames)
		)
	return(out)
}

# Start with 100 healthy people
p.start <- matrix(
	c(100, 0, 0,
	  0, 0, 0,
	  0, 0, 0
	),
	nrow=3, ncol=3, byrow=TRUE, dimnames=list(statesNames, statesNames)
)

myps <- KFE.pijt( horizon=50, h=1, population.start=p.start, ForcesofMort=mus, statesNames=statesNames  )

mus(50)

# Some disabed people become healthy, but eventually most die
a <- round(myps[,"Healthy",],5)
a
plot(a[,2])

#plot(a[,1])
#plot(a[,3])