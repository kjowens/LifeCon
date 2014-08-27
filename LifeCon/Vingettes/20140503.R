# q <- c(.2,.5,.3)
# m <- 2
# FAA <- 2
# i <- .05
# am(q,m,FAA,i)
# .5*1 + .5*(1-.5*.1)/1.05^.5
# f <- c(.2,.5,.3)
# HuffmanCode(f)
# dim(f)
# class(f)
# x <- WalkerTable(f)
# 

# q <- c(.2,.5,.3)

m <- 2
FAA <- 2
i <- .05
k <- 5

q <- c(.3,.4,1)
# f <- SK(q)[1:n]*q
# f
# it needs to have a terminal age
# sum(SK(q)[1:n]*q)
# f[1]+f[2]

y <- WalkerTable(q)
y
sum(y[,1])

set.seed(42)
RGD(y,k)
set.seed(42)
m <- runif(k)
m[1]
m[1]*3
ceiling(m[1]*3)
y