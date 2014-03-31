# LCcheck.q(c(.1,-1))
# LCcheck.q(c(.1,.1))
# LCcheck.q(c(.1,1))
# 
# LCcheck.FAA(0)
# LCcheck.FAA(1)
# LCcheck.FAA(2)
# LCcheck.FAA(3)
# LCcheck.FAA(4)

qOY2qmthly(.1,0,2)
prod(qOY2qmthly(.1,2,0) == c(.1,0))
prod(qOY2qmthly(.1,2,1) == c(0,.1))
prod(qOY2qmthly(.1,2,2)==c(.05, (.1/2)/(1-.1/2)))
prod(qOY2qmthly(c(.1,.2),2,2)==c(.05,
								(.1/2)/(1-.1/2),
								 .1,
								 (.2/2)/(1-.2/2))
)
prod(qOY2qmthly(.1,2,3)==rep(1-.9^.5,2))
prod(qOY2qmthly(c(.1,.2),2,3)==c(rep(1-.9^.5,2),
								 rep(1-.8^.5,2))
)
