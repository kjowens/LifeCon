## The following three functions are related to accessing SoA database of mortality tables
## TbSearch, TbDetails, Tb
## Of these TbSearch and Tb are meant to be public functions and TbDetails private.
## One has to have the tables.dat and tables.ndx files in the working directory

        ## Allows you to search the database
        ## Useful to identify the Table No.

        TbSearch<-function(NameStr,CountryStr,UsageStr)
            {
                zz<-file("tables.ndx","rb");
                readChar(zz,58);
                k<-1;
                for (i in 1:((file.info("tables.ndx")$size-58)/90))
                {
                    TbNo=readBin(zz,integer(),size=4);
                    TbName=readChar(zz,50);
                    TbOffset=readBin(zz,integer(),size=4);
                    TbCountry=readChar(zz,31);
                    TbUsage=readBin(zz,integer(),size=1);
                    if (any(grep(NameStr,TbName,ignore.case=TRUE,perl=TRUE)) && any(grep(CountryStr,TbCountry,ignore.case=TRUE,perl=TRUE)) &&
                        any(grep(UsageStr,as.character(TbUsage),ignore.case=TRUE,perl=TRUE))
                    )
                    {
                        if (k==1)
                        {
                            No<-as.vector(TbNo);
                            Name<-as.vector(TbName);
                            Country<-as.vector(TbCountry);
                            Usage<-as.vector(TbUsage);
                        }
                        else
                        {
                            No[k]=TbNo;
                            Name[k]=TbName;
                            Country[k]=TbCountry;
                            Usage[k]=TbUsage;
                        }
                        k=k+1;
                    }
                }
                close(zz);
                if (k!=1)
                {
                    dummy<-data.frame(No=No,Name=Name,Country=Country,Usage=Usage);
                    #dummy<-dummy[order(dummy$Country,dummy$Usage,dummy$No), , , ];
                    dummy<-list(Country=dummy$Country,Usage=dummy$Usage,Name=dummy$Name,No=dummy$No);
                }
                else
                {
                    dummy<-list(Messages="Search Resulted in Zero Entries");
                }
                data.frame(dummy)
            }

        ## Reads the index - Auxilliary Function for Tb

        TbDetails<-function(TbSearch)
            {
                zz<-file("tables.ndx","rb");
                readChar(zz,58);

                TbNo=0;
                while (TbNo!=TbSearch)
                {
                    TbNo=readBin(zz,integer(),size=4);
                    if (TbNo!=TbSearch)
                        {
                            readChar(zz,86,useBytes = TRUE);
                        }
                    else
                        {
                            TbName=readChar(zz,50);
                            TbOffset=readBin(zz,integer(),size=4);
                            TbCountry=readChar(zz,31);
                            TbUsage=readBin(zz,integer(),size=1);
                        }
                }
                close(zz);
                list(No=TbNo,Offset=TbOffset,Usage=TbUsage,Name=TbName,Country=TbCountry)
            }

        ## Gets a Table from the SOADB in the form of a nice list
        ## The structure of the list is apparent from the final lines of code of Tb
        ## This depends on TbDetails
        ## The index and data file are assumed to be in the working directory
        ## The file names have been assumed to be the same as they are distributed by the SOA

		Tb <- function(TbSearch)
		            {
		                TbOffset<-TbDetails(TbSearch)["Offset"];
		                RNo<-0;

		                zz<-file("tables.dat","rb");
		                readChar(zz,TbOffset,useBytes = TRUE);

		                while (RNo!=9999)
		                {
		                    RNo=readBin(zz,integer(),size=2);
		                    if (RNo!=9999)
		                    {
		                        RLen = readBin(zz,integer(),size=2);
		                        if (RNo==2)
		                            { ##32-bit integer
		                                TbNo=readBin(zz,integer(),size=4);
		                            }
		                        else if ((RNo<=11) || (RNo==19))
		                            { ## Text Fields
		                                if (RNo<=11)
		                                {
		                                    switch(RNo,
		                                        TbName<-readChar(zz,RLen,useBytes = TRUE),
		                                        ,
		                                        TbType<-readChar(zz,RLen,useBytes = TRUE),
		                                        TbContributor<-readChar(zz,RLen,useBytes = TRUE),
		                                        TbSource<-readChar(zz,RLen,useBytes = TRUE),
		                                        TbVolume<-readChar(zz,RLen,useBytes = TRUE),
		                                        TbObsn<-readChar(zz,RLen,useBytes = TRUE),
		                                        TbUnit<-readChar(zz,RLen,useBytes = TRUE),
		                                        TbMethod<-readChar(zz,RLen,useBytes = TRUE),
		                                        TbRef<-readChar(zz,RLen,useBytes = TRUE),
		                                        TbMisc<-readChar(zz,RLen,useBytes = TRUE)
		                                    )

		                                }
		                                else
		                                {
		                                    TbCountry<-readChar(zz,RLen,useBytes = TRUE);
		                                }
		                            }
		                        else if (RNo<= 16)
		                            { ## 16-bit integer
		                                switch((RNo-11),
		                                    MinAge<-readBin(zz,integer(),size=2),
		                                    MaxAge<-readBin(zz,integer(),size=2),
		                                    SelPrd<-readBin(zz,integer(),size=2),
		                                    MaxSAge<-readBin(zz,integer(),size=2),
		                                    NoDec<-readBin(zz,integer(),size=2)
		                                    )
		                            }
		                        else if (RNo==17)
		                            { ## 8-byte IEEE floating point
		                                qa<-as.array(readBin(zz,double(),n=RLen/8,size=8));
		                            }
		                        else if (RNo==18)
		                            { ## 32-bit unsigned integer
		                                TbHash=readBin(zz,integer(),size=4);
		                            }
		                        else
		                            { ## Short
		                                TbUse=readBin(zz,integer(),size=1);
		                            }

		                    }
		                    else
		                    { ## Re-Format q
		                    ## Three cases - Aggegrate
		                    ##             - Select - MaxSAge = MaxAge
		                    ##             - Select - MaxSAge <MaxAge
		                        if (SelPrd>0)
		                        {
		                            if (MaxSAge<MaxAge)
		                            {
		                                ## Just the Select Rates
		                                    qs<-t(array(qa[c(1:(SelPrd*(MaxSAge-MinAge+1))) + trunc(c(0:(SelPrd*(MaxSAge-MinAge+1)-1))/SelPrd)],c(SelPrd,(MaxSAge-MinAge+1))));
		                                ## Just the Ultimate Rates
		                                    qu<-array(qa[c((SelPrd+1)*c(1:(MaxSAge-MinAge+1)),(SelPrd+1)*(MaxSAge-MinAge+1)+c(1:(MaxAge-MaxSAge)))],c(MaxAge-(MinAge+SelPrd)+1));

		                            }
		                            else
		                            {
		                                ## Just the Select Rates
		                                    qs<-t(array(qa[c(1:(SelPrd*(MaxSAge-MinAge+1))) + trunc(c(0:(SelPrd*(MaxSAge-MinAge+1)-1))/SelPrd)],c(SelPrd,(MaxSAge-MinAge+1))));
		                                ## Just the Ultimate Rates
		                                    qu<-array(qa[(SelPrd+1)*c(1:(MaxSAge-MinAge+1))],c(MaxAge-(MinAge+SelPrd)+1));
		                            }
		                        }
		                        else
		                        {
		                            ## Do Nothing
		                        }
		                    }
		                }
		                ##Output Section
		                if (inherits(try(TbSource,TRUE),'try-error'))
		                {
		                    TbSource<-"Missing Field";
		                }
		                if (inherits(try(TbVolume,TRUE),'try-error'))
		                {
		                    TbVolume<-"Missing Field";
		                }
		                if (inherits(try(TbObsn,TRUE),'try-error'))
		                {
		                    TbObsn<-"Missing Field";
		                }                
		                if (inherits(try(TbUnit,TRUE),'try-error'))
		                {
		                    TbUnit<-"Missing Field";
		                }                
		                TbData<-list(Country=TbCountry,Source=TbSource,Volume=TbVolume,Obsn=TbObsn,Unit=TbUnit);
		                if (inherits(try(TbMethod,TRUE),'try-error'))
		                {
		                    TbMethod<-"Missing Field";
		                }                
		                if (inherits(try(TbRef,TRUE),'try-error'))
		                {
		                    TbRef<-"Missing Field";
		                }                
		                if (inherits(try(TbMisc,TRUE),'try-error'))
		                {
		                    TbMisc<-"Missing Field";
		                }                                
		                TbMetaData<-list(Name=TbName,Type=TbType,Use=TbUse,Method=TbMethod,NoDec=NoDec,Ref=TbRef,Misc=TbMisc);
		                if (TbType=="S")
		                    {
		                        TbValues<-list(Meta=list(Type=TbType,MinAge=MinAge,MaxAge=MaxAge,SelPrd=SelPrd,MaxSAge=MaxSAge),Select=qs, Ultimate=qu);
		                    }
		                    else
		                    {
		                        TbValues<-list(Meta=list(Type=TbType,MinAge=MinAge,MaxAge=MaxAge),Aggregate=qa);
		                    }
		                    close(zz);
		                list(Data=TbData,MetaData=TbMetaData,Values=TbValues)
		            }

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
                
            RGD<-function(T,k)
                {
                    n<-length(T[,1]);
                    r<-as.array(runif(k));
                    for (l in 1:k)
                    {
                        u<-r[l];
                        i<-ceiling(n*u);
                        if (u>=T[i,1]) {
                            r[l]=i;
                        }
                        else {
                            r[l]=T[i,2];
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
                
            RT<-function(q,FracAgeAssmpn,k)
                {
                    n<-length(q);
                    T<-WalkerTable(SK(q)[1:n]*q);
                    r<-as.array(runif(k));
                    for (l in 1:k)
                    {
                        u<-r[l];
                        i<-ceiling(n*u);
                        if (u>=T[i,1]) {
                            r[l]=i-1+IDF(q[i],FracAgeAssmpn,(u-T[i,1])/(1-T[i,1]));
                        }
                        else {
                            r[l]=T[i,2]-1+IDF(q[T[i,2]],FracAgeAssmpn,u/T[i,1]);
                            }
                    }
                    r   
                }

        ## Inverse Distribution Function for fractional age assumptions
        
            IDF<-function(q,FracAgeAssmpn,x)
                {
                    if (FracAgeAssmpn==0)
                        {
                            0
                        }
                    else if (FracAgeAssmpn==1)
                        {
                            1
                        }
                    else if (FracAgeAssmpn==2)
                        {
                            x
                        }
                    else if (FracAgeAssmpn==3)
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
##      a(q, FracAgeAssmpn)
##          This gives E(T(x)|T(x)<1) - q->q_x - for the FracAgeAssmpn
##          Useful for computing complete future life times
##      M(q, FracAgeAssmpn,interest rate)
##          This gives E(exp(-\delta T(x))|T(x)<1) - q->q_x - for the FracAgeAssmpn and \delta=ln(1+interest rate)
##          Note that E(exp(-\delta T(x))|T(x)<1)q_x is the APV of a one year term insurance on age x
##          Note that E(exp(-\delta T(x))|T(x)<1)q_x + \nu*p_x is the APV of a one year endowment insurance on age x
##          Useful for computing continuous insurances and continuous annuities
##      am(q,m,FracAgeAssmpn,i)
##          The following function helps in computing one year temporary annuity due payable m-thly under any
##          of the standard fractional age assumptions. Using it we define below the OYT payable m-thly.
##      OYT(q,m,FracAgeAssmpn,i)
##          OYT i.e. One Year Term insurance is valued using the function M and the function am

        ##      a(q, FracAgeAssmpn)
        ##          This gives E(T(x)|T(x)<1) - q->q_x - for the FracAgeAssmpn
        ##          Useful for computing complete future life times

                        a<-function(q,FracAgeAssmpn)
                            {
                                if (FracAgeAssmpn==0)
                                {
                                    dum<-function(q)
                                    {
                                        0
                                    }
                                }
                                else if (FracAgeAssmpn==1)
                                {
                                    dum<-function(q)
                                    {
                                        1
                                    }
                                }
                                else if (FracAgeAssmpn==2)
                                {
                                    dum<-function(q)
                                    {
                                        0.5
                                    }
                                }
                                else if (FracAgeAssmpn==3)
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
                                else if (FracAgeAssmpn==4)
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

        ##      M(q, FracAgeAssmpn,interest rate)
        ##          This gives E(exp(-\delta T(x))|T(x)<1) - q->q_x - for the FracAgeAssmpn and \delta=ln(1+interest rate)
        ##          Note that E(exp(-\delta T(x))|T(x)<1)q_x is the APV of a one year term insurance on age x
        ##          Note that E(exp(-\delta T(x))|T(x)<1)q_x + \nu*p_x is the APV of a one year endowment insurance on age x
        ##          Useful for computing continuous insurances and continuous annuities

                        M<-function(q,FracAgeAssmpn,i)
                            {
                                if (FracAgeAssmpn==0)
                                {
                                    dum<-function(q)
                                    {
                                        1
                                    }
                                }
                                else if (FracAgeAssmpn==1)
                                {
                                    dum<-function(q)
                                    {
                                        1/(1+i)
                                    }
                                }
                                else if (FracAgeAssmpn==2)
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
                                else if (FracAgeAssmpn==3)
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
                                else if (FracAgeAssmpn==4)
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

                        am<-function(q,m,FracAgeAssmpn,i)
                        {
                            delta<-log(1+i);
                            disc<-i/(1+i);
                            if (m!=0)
                            {
                                im<-m*((1+i)^(1/m)-1);
                                dm<-m*im/(m+im);
                            }
                            if (FracAgeAssmpn==0)
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
                            else if (FracAgeAssmpn==1)
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
                            else if (FracAgeAssmpn==2)
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
                            else if (FracAgeAssmpn==3)
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
                            else if (FracAgeAssmpn==4)
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


                        OYT<-function(q,m,FracAgeAssmpn,i)
                        {
                            if (m!=0)
                            {
                                dm<-m*(1-(1+i)^(-1/m));
                                Soln<-1-dm*am(q,m,FracAgeAssmpn,i)-(1-q)/(1+i);
                            }
                            else
                            {
                                Soln<-q*M(q,FracAgeAssmpn,i);
                            }
                            Soln
                        }
