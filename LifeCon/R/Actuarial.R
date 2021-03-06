# 2014-07-06, KJO: core actuarial functions

        ## Inverse Distribution Function for fractional age assumptions
        
            IDF<-function(q,FAA,x)
                {
                    if (FAA==0)
                        {
                            0
                        }
                    else if (FAA==1)
                        {
                            1
                        }
                    else if (FAA==2)
                        {
                            x
                        }
                    else if (FAA==3)
                        {
                            if (q<0.000001)
                                {
                                    x+0.5*q*x*(x-1)
                                }
                            else if (q==1)
                                {
                                    0
                                }
                            else
                                {
                                    log(1-q*x)/log(1-q)
                                }
                        }
                    else 
                        {

                            (1-q)*x/(1-q*x)
                        }
                }
        

## Backward Linear First Order Recurrence - takes care of insurance
## Final boundary value is bv
## The recurrence is x_n = r_n + s_n * x_{n+1}
## Handles Annuity & Insurance Recurrence; Fackler; Hattendorff;
## Must be implemented in C

        rBLFOR<-function(r,s,bv)
            {
                temp<-bv;
                dum<-function(x){
                    temp<<-x[1]+x[2]*temp
                }
                as.array(apply(cbind(array(r,dim=length(s)),array(s))[length(s):1,],1,FUN=dum)[length(s):1]);
            }

## The following function finds the p vector for a given q vector
## S stands for the survival function and K for the curtate future life time

        SK<-function(q)
            {
                temp<-1;
                dum<-function(x){
                    temp<<-(1-x)*temp
                }
                unlist(lapply(c(0,q),FUN=dum))
            }

## Fractional Age Assumptions related functions
## The fractional age assumptions considered are the following :
##      0 - Degenerate at 0
##      1 - Degenerate at 1
##      2 - Uniform(0,1)
##      3 - Exponential
##      4 - Hyperboilic
## The functions defined are the following:
##      a(q, FAA)
##          This gives E(T(x)|T(x)<1) - q->q_x - for the FAA
##          Useful for computing complete future life times
##      M(q, FAA,interest rate)
##          This gives E(exp(-\delta T(x))|T(x)<1) - q->q_x - for the FAA and \delta=ln(1+interest rate)
##          Note that E(exp(-\delta T(x))|T(x)<1)q_x is the APV of a one year term insurance on age x
##          Note that E(exp(-\delta T(x))|T(x)<1)q_x + \nu*p_x is the APV of a one year endowment insurance on age x
##          Useful for computing continuous insurances and continuous annuities
##      am(q,m,FAA,i)
##          The following function helps in computing one year temporary annuity due payable m-thly under any
##          of the standard fractional age assumptions. Using it we define below the OYT payable m-thly.
##      OYT(q,m,FAA,i)
##          OYT i.e. One Year Term insurance is valued using the function M and the function am

        ##      a(q, FAA)
        ##          This gives E(T(x)|T(x)<1) - q->q_x - for the FAA
        ##          Useful for computing complete future life times

                        a<-function(q,FAA)
                            {
                                if (FAA==0)
                                {
                                    dum<-function(q)
                                    {
                                        0
                                    }
                                }
                                else if (FAA==1)
                                {
                                    dum<-function(q)
                                    {
                                        1
                                    }
                                }
                                else if (FAA==2)
                                {
                                    dum<-function(q)
                                    {
                                        0.5
                                    }
                                }
                                else if (FAA==3)
                                {
                                    dum<-function(q)
                                    {
                                        if (q==1)
                                        {
                                            Soln<- 0;
                                        }
                                        else if (q<0.000001)
                                        {
                                            Soln<- 0.5 - q/12;
                                        }
                                        else
                                        {
                                            Soln<- -((1-q)/q + 1/log(1-q));
                                        }
                                        Soln
                                    }
                                }
                                else if (FAA==4)
                                {
                                    dum<-function(q)
                                    {
                                        if (q==1)
                                        {
                                            Soln<-0
                                        }
                                        else if (q<0.000001)
                                        {
                                            Soln<-0.5 - q/6
                                        }
                                        else
                                        {
                                            Soln<- -((1-q)/q^2) * (q + log(1-q))
                                        }
                                        Soln
                                    }
                                }
                                else
                                {
                                    dum<-function(q)
                                    {
                                        NA
                                    }
                                }
                            unlist(lapply(q,dum))
                        }

        ##      M(q, FAA,interest rate)
        ##          This gives E(exp(-\delta T(x))|T(x)<1) - q->q_x - for the FAA and \delta=ln(1+interest rate)
        ##          Note that E(exp(-\delta T(x))|T(x)<1)q_x is the APV of a one year term insurance on age x
        ##          Note that E(exp(-\delta T(x))|T(x)<1)q_x + \nu*p_x is the APV of a one year endowment insurance on age x
        ##          Useful for computing continuous insurances and continuous annuities

                        M<-function(q,FAA,i)
                            {
                                if (FAA==0)
                                {
                                    dum<-function(q)
                                    {
                                        1
                                    }
                                }
                                else if (FAA==1)
                                {
                                    dum<-function(q)
                                    {
                                        1/(1+i)
                                    }
                                }
                                else if (FAA==2)
                                {
                                    if (i<0.000001 )
                                    {
                                        dum<-function(q)
                                        {
                                            1-i/2
                                        }
                                    }
                                    else
                                    {
                                        dum<-function(q)
                                        {
                                            (1-1/(1+i))/log(1+i)
                                        }
                                    }
                                }
                                else if (FAA==3)
                                {
                                    dum<-function(q)
                                    {
                                        if (q>0.000001 && q!=1)
                                        {
                                            Soln<- -(log(1-q)*(1-(1-q)/(1+i)))/(q*(-log(1-q)+log(1+i)));
                                        }
                                        else if (q<1-0.999999*(1+i))
                                        {
                                            Soln<- (1+q/2)/(1.5-0.5*(1-q)/(1+i));
                                        }
                                        else if (q<0.000001)
                                        {
                                            Soln<- (1+q/2)*(1-(1-q)/(1+i));
                                        }
                                        else
                                        {
                                            Soln<- 1;
                                        }
                                        Soln
                                    }
                                }
                                else if (FAA==4)
                                {
                                    delta<-log(1+i);
                                    dum<-function(q)
                                    {
                                        if (q>0.99999)
                                        {
                                            if (q==1)
                                            {
                                                Soln<-1;
                                            }
                                            else
                                            {
                                                f1<-function(x)
                                                {
                                                    (exp(-delta*x)-1+delta*x)/(1-q+q*x)^2
                                                }
                                                Soln<1+integrate(f1,0,1)$value*(1-q)+delta*((1-q)/q^2) * (q + log(1-q));
                                            }
                                        }
                                        else
                                        {
                                            f2<-function(x)
                                            {
                                                exp(-delta*x)/(1-q+q*x)^2
                                            }
                                            Soln<-(1-q)*integrate(f2,0,1)$value;
                                        }
                                        Soln
                                    }
                                }
                                else
                                {
                                    dum<-function(q)
                                    {
                                        NA
                                    }
                                }
                                unlist(lapply(q,dum))
                            }

        ## The following function helps in computing one year temporary annuity due payable m-thly under any
        ## of the standard fractional age assumptions. Using it we define below the OYT payable m-thly.

                        am<-function(q,m,FAA,i)
                        {
							# Pays 1 annually
                            delta<-log(1+i);
                            disc<-i/(1+i);
                            if (m!=0)
                            {
                                im<-m*((1+i)^(1/m)-1);
                                dm<-m*im/(m+im);
                            }
                            if (FAA==0)
                            {
                                if (m==0)
                                {
                                    dum<-function(q)
                                    {
                                        (1-q)*disc/delta
                                    }
                                }
                                else
                                {
                                    dum<-function(q)
                                    {
                                        q/m+(1-q)*disc/dm
                                    }
                                }
                            }
                            else if (FAA==1)
                            {
                                if (m==0)
                                {
                                    dum<-function(q)
                                    {
                                        ## Annuity Certain - Continuous

                                        disc/delta
                                    }
                                }
                                else
                                {
                                    dum<-function(q)
                                    {
                                        ## Annuity Certain - Due payable m-thly

                                        disc/dm
                                    }
                                }
                            }
                            else if (FAA==2)
                            {
                                if (m==0)
                                {
                                    dum<-function(q)
                                    {
                                        q/delta*(1-disc/delta)+(1-q)*disc/delta
                                    }
                                }
                                else
                                {
                                    dum<-function(q)
                                    {
                                        q/dm*(1-disc/im)+(1-q)*disc/dm
                                    }
                                }
                            }
                            else if (FAA==3)
                            {
                                if (m==0)
                                {
                                    dum<-function(q)
                                    {
                                        Soln<-0;
                                        if ((1-q)/(1+i)!=1)
                                        {
                                            Soln<-((1-q)/(1+i)-1)/(log((1-q)/(1+i)));
                                        }
                                        Soln
                                    }
                                }
                                else
                                {
                                    dum<-function(q)
                                    {
                                        Soln<-1/m;
                                        if ((1-q)/(1+i)!=1)
                                        {
                                            Soln=(1-(1-q)/(1+i))/(m*(1-((1-q)/(1+i))^(1/m)));
                                        }
                                        Soln
                                    }

                                }
                            }
                            else if (FAA==4)
                            {
                                if (m==0)
                                {
                                    dum<-function(q)
                                    {
                                        if (q>0.99999)
                                        {
                                            if (q==1)
                                            {
                                                Soln<-0;
                                            }
                                            else
                                            {
                                                f1<-function(x)
                                                {
                                                    (1-exp(-delta*x)-delta*x)/(1-q+q*x)^2
                                                }
                                                Soln<-integrate(f1,0,1)$value*(1-q)/delta-((1-q)/q^2) * (q + log(1-q));
                                            }
                                        }
                                        else
                                        {
                                            f2<-function(x)
                                            {
                                                exp(-delta*x)/(1-q+q*x)^2
                                            }
                                            Soln<-(1-(1-q)/(1+i)-q*(1-q)*integrate(f2,0,1)$value)/delta;
                                        }
                                        Soln
                                    }
                                }
                                else
                                {
                                    dum<-function(q)
                                    {
                                        Soln<-1;
                                        for (k in 1:(m-1))
                                        {
                                            Soln=Soln+(1-q)/((1-(1-k/m)*q)*(1+im/m)^k)
                                        }
                                        Soln/m
                                    }
                                }
                            }
                            else
                            {
                                dum<-function(q)
                                {
                                    NA
                                }
                            }
                            unlist(lapply(q,dum))
                        }


        ## OYT i.e. One Year Term insurance is valued using the function M and the function am


                        OYT<-function(q,m,FAA,i) {
                            if (m!=0) {
                                dm<-m*(1-(1+i)^(-1/m));
                                Soln<-1-dm*am(q,m,FAA,i)-(1-q)/(1+i);
							} else {
                                Soln<-q*M(q,FAA,i);
                            }
                            Soln
                        }


##########
# 2014-03-30
LCcheck.q <- function(q) {
	if(prod(q>0)==0) {stop("at least one element of q is less than zero")}
	# else if(q[length(q)]!=1) {warning("the last element in q is not 1")}
}

LCcheck.FAA <- function(FAA) {
	if(! FAA %in% c(0,1,2,3,4)) {stop("The fractional age assumption that was entered is not one of the permitted assumptions. They are:
		0 - Degenerate at 0
		1 - Degenerate at 1
		2 - Uniform(0,1)
		3 - Exponential
		4 - Hyperboilic")}
}



qOY2qmthly_OY <- function(q,m,FAA) {
	# Converts a single one-year death probabily into a vector of mthly death probabilities
	# m is the number of periods in each year
	# h is the length of each period
	#  the function returns:
		 # for k = 0, 1, ... , m-1, Pr(T < (k+1)*h | T >= k*h)
	if(round(m,0)!=m) {warning("m is not an integer, rounding it to the closest integer")}
		m <- round(m,0)
	if(m==0) {stop("m can't be zero")}
	LCcheck.q(q)
	LCcheck.FAA(FAA)
	out <- rep(NA, m*length(q))
	if(FAA==0) {
		out <- c(q, rep(0, m*length(q)-1))
	}
	else if(FAA==1) {
		out <- c(rep(0, m*length(q)-1), q)
	}
	h <- 1/m # h is used in the uniform and exponential fractional age assumptions
	if(FAA==2) {
		temp2 <- q*h
		for (k in 0:(m-1)) {
			out[k+1] <- temp2/(1 - k*temp2)
		}
	}
	else if(FAA==3) {
		out <- rep(1 - (1-q)^h, m*length(q))
	}
	else if(FAA==4) {
		stop("The Hyperbolic fractional age assumption is not supported at this time.")
	}
	return(out)
}

qOY2qmthly <- function(q,m,FAA) {
	# converts a VECTOR of one-year death probabilities to a vector of mthly 1/m duration death probabilities
	unlist(lapply(q,FUN = function(x) qOY2qmthly_OY(q=x,m=m,FAA=FAA)))
}

