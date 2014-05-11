## The following two functions help simulate K (and hence T)
## The first function finds the Huffman optimal tree for a given discrete distribution
## The second function simulates a random vector of size k from the distribution which
## corresponds to the given Huffman tree.

        ## The following function finds the Huffman optimal tree for a given discrete distribution
        ## To be used only once for a given mortality table

            HuffmanCode<-function(f)
                {
                    n<-dim(f);
                    Node<-n;
                    Forest<-cbind(f,1:n);
                    Tree<-array(0,c(n,4));
                    Parents<-array(0,c(3*n,20))
                    NP<-array(1,3*n);
                    Tree[1:dim(f),4]=f;
                    while (n>1)
                    {
                        x<-order(Forest[,1])[1:2];
                        Tree<-rbind(Tree,array(c(Forest[x[1],2],Forest[x[2],2],0,Forest[x[1],1]+Forest[x[2],1]),c(1,4)));
                        Node=Node+1;
                            Tree[Forest[x[1],2],3]=Node;
                            Tree[Forest[x[2],2],3]=Node;
                            Tree[Forest[x[2],2],4]=Forest[x[1],1]+Forest[x[2],1];
                            Parents[Forest[x[1],2],NP[Forest[x[1],2]]]=Node;
                            NP[Forest[x[1],2]]=NP[Forest[x[1],2]]+1;
                            Parents[Forest[x[2],2],NP[Forest[x[2],2]]]=Node;
                            NP[Forest[x[2],2]]=NP[Forest[x[2],2]]+1;
                        for (k in 1:Node)
                        {
                            if ((any(Parents[k,]==Forest[x[1],2])) || (any(Parents[k,]==Forest[x[2],2])))
                            {
                                Parents[k,NP[k]]=Node;
                                NP[k]=NP[k]+1;
                                if (any(Parents[k,]==Forest[x[2],2]))
                                {
                                    Tree[k,4]=Tree[k,4]+Forest[x[1],1];
                                }
                            }
                        }   
                        Forest[x[1],]<-array(c(Forest[x[1],1]+Forest[x[2],1],Node),c(1,2));
                        n=n-1;
                        Forest<-array(Forest[Forest[,1]!=Forest[x[2],1]],c(n,2));
                    }
                    Tree
                }

        ## The following function builds the tables for the Alias Method of Walker
        
            WalkerTable<-function(f)
                {
					# f <- SK(q)[1:n]*q
                    F<-cbind(length(f)*f,1:length(f));
                    L<-array(0,length(f));
                    while (length(F[(F[,1]<1)&(F[,1]>0),1])>0) 
                        {
                            j<-F[(F[,1]<1)&(F[,1]>0),2][1];
                            k<-F[F[,1]>=1,2][1];
                            L[j]=k;
                            F[k,1]=F[k,1]-1+F[j,1];
                            F[j,1]= - F[j,1];
                        }
                    cbind((F[,2]-abs(F[,1]))/length(f),L)
                }

        ## The following function simulates from a general discrete distribution using the alias method of Walker.
                
            RGD<-function(tbl,k)
                {
                    n<-length(tbl[,1]);
                    r<-as.array(runif(k));
                    for (l in 1:k)
                    {
                        u<-r[l];
                        i<-ceiling(n*u);
                        if (u>=tbl[i,1]) {
                            r[l]=i;
                        }
                        else {
                            r[l]=tbl[i,2];
                            }
                    }
                    r
                }

        ## The following function simulates a random vector of size k from the distribution which
        ## corresponds to the given Huffman tree. Huffman tree can be found using the above function
        ## and in fact the two can be combined as
        ##                                          RGD(HuffmanCode(f),k)
        ##

            RGDHuffman<-function(Tree,k)
                {
                    n<-length(Tree)/4;
                    m<-length(Tree[Tree[,1]==0])/4;
                    HuffmanDecode<-function(r)
                        {
                            nn<-n;
                            while (nn>m)
                            {
                                if (r<Tree[Tree[nn,1],4])
                                {
                                    nn=Tree[nn,1];
                                }
                                else
                                {
                                    nn=Tree[nn,2];
                                }
                            }
                            nn
                        }
                    r<-as.array(runif(k));
                    for (l in 1:k)
                    {
                       r[l]<-HuffmanDecode(r[l]);
                    }
                    r                        
                }

        ## The following function simulates from a general discrete distribution using the alias method of Walker.
                
            RT<-function(q,FAA,k)
                {
                    n<-length(q);
                    T<-WalkerTable(SK(q)[1:n]*q);
                    r<-as.array(runif(k));
                    for (l in 1:k)
                    {
                        u<-r[l];
                        i<-ceiling(n*u);
                        if (u>=T[i,1]) {
                            r[l]=i-1+IDF(q[i],FAA,(u-T[i,1])/(1-T[i,1]));
                        }
                        else {
                            r[l]=T[i,2]-1+IDF(q[T[i,2]],FAA,u/T[i,1]);
                            }
                    }
                    r   
                }

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

        BLFOR<-function(r,s,bv)
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


###
# 2014-05-03
# Some useful functions
.ones <- function(temp) {as.array(rep(1, length(temp)))}

.DiscountFact <- function(i, m) {
	# returns a discount factor based on an anual effective interest rate
	if(m==0) {stop("The frequency, 'm' can't be zero")} else {
		out <- NULL
		out <- {1+i}^-{1/m}
	}
	return(out)
}

BenFactors <- function(OYq, i, PB=PolBasis) {
	# TODO: change to use OYT, one year term
	out <- NULL
	end <- PB$SurvFactor
	freq <- PB$BenFreq
	q <- qOY2qmthly(q=OYq, m=freq, FAA=PB$FAA)
	v <- .DiscountFact(i,m=freq) * .ones(freq*PB$TermDur)
	out <- BLFOR(q*v, (1-q)*v, end)
	return(out)
}

PremFactors <- function(OYq, i, PB=PolBasis) {
	out <- NULL
	end <- 0 # not reasonable for them to pay a premium at the end
	freq <- PB$PremFreq
	q <- qOY2qmthly(q=OYq, m=freq, FAA=PB$FAA)
	v <- .DiscountFact(i,m=freq) * .ones(freq*PB$TermDur)
	out <- BLFOR(.ones(q), (1-q)*v, end)
	return(out)
}

.GetPolDur <- function(Date, IssueDate) {
	out <- NULL
	out <- Date - IssueDate
	return(out)
}


# get parameter value
.gpv <- function(df, param, type="numb") {
	out <- NULL
	bar <- df[which(df$Parameter==param),"Value"]
	out <- switch(type, numb=as.numeric(levels(bar)[bar]),
						char=paste(levels(bar)[bar])
				)
	return(out)
}

.YM2date <- function(year,month) {
	out <- year + (month - 1)/12
	return(out)
}

setProjParams <- function(df) {
	out <- list()
	# I'm going to use begining of the month as the standard
	# i.e., Jan 2015 means 1/1/2015, or 2015.00; 12/31/2014 in some other programs
	out$PjTimeUnits <- .gpv(df,"ProjTimeUnits","char")
	out$PjSY <- .gpv(df,"ProjStartYear")
	out$PjSM <- .gpv(df,"ProjStartMonth")
	out$PjSD <- .YM2date(out$PjSY,out$PjSM) # as float in years
	out$PjEY <- .gpv(df,"ProjEndYear")
	out$PjEM <- .gpv(df,"ProjEndMonth")
	out$PjED <- .YM2date(out$PjEY,out$PjEM) # as float in years
	out$PjDur <- out$PjED - out$PjSD # Projection Duration
	# Projection frequency
	#  1 <- annual
	#  2 <- semi-annual
	#  3 <- every four months
	#  4 <- quarterly
	# 12 <- monthly
	out$PjFreq <- .gpv(df,"ProjFreq")
	out$PjDates <- seq(out$PjSD,out$PjED,out$PjFreq)
	out$ProjTimes <- seq(0,out$PjDur,out$PjFreq)
	return(out)
}


