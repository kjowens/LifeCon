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
	# policy holder pv benefit factors
	out <- NULL
	end <- PB$SurvFactor # 0 for term, 1 for WL, endowment ins.
	freq <- PB$BenFreq
	q <- qOY2qmthly(q=OYq, m=freq, FAA=PB$FAA)
	v <- .DiscountFact(i,m=freq) * .ones(freq*PB$TermDur)
	out <- BLFOR(q*v, (1-q)*v, end)
	return(out)
}

BenFactors2 <- function(OYq, i, PB=PolBasis) {
	# TODO: change to use OYT, one year term
	# policy holder pv benefit factors
	out <- NULL
	end <- PB$SurvFactor # 0 for term, 1 for WL, endowment ins.
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



.gpv <- function(df, param, type="numb") {
	# get parameter value
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
