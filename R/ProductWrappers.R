# let's grab this guy first

## Backward Linear First Order Recurrence - takes care of insurance
## Final boundary value is bv
## The recurrence is x_n = r_n + s_n * x_{n+1}
## Handles Annuity & Insurance Recurrence; Fackler; Hattendorff;
## Must be implemented in C

        BLFOR<-function(r,s,bv)
            {
                temp<-bv;
                dum<-function(x){
                    temp<<-x[1]+x[2]*temp
                }
                as.array(apply(
					cbind(
						array(r,dim=length(s)),
						array(s)
					)[length(s):1,],1,FUN=dum)[length(s):1]);
            }


# using results from Makeham.R
myp
qOY

qOY <- c(.5,.5)
# some interest rate
i <- 0.05

# DR for Decrement Rule
mybasis <- list(ages=50, i=i, DR=qOY, DRinterphow="qOY", iinterphow="flat")
ins.term <- function(basis=mybasis, x=0, deffered = 0, term=10, m=1) {
	# note: deffered, term, and m not yet implemented
	if(basis$DRinterphow=="qOY"){
		q <- basis$DR
	}
	if(basis$iinterphow=="flat") {
		v <- 1/(1+basis$i)
	}
	out <- BLFOR(r=q*v,s=(1-q)*v,0)
	return(out)
}
ins.term()

# simple example: 2 year term insurance, q for both years is .5, i=.05
.5/1.05
.25/1.05^2 + .5/1.05