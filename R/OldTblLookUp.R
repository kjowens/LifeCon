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
