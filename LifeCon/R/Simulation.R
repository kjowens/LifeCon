# 2014-07-06, KJO: functions for Monte Carlo simulation

## The following two functions help simulate K (and hence T)

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