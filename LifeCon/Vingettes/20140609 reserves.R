ages <- seq(25, 50, 5)

tbl <- SOAtbl2df.agg(15006)
tbl


q<-unlist(tbl$qx);
x<-30;n<-30;i<-.04;delta<-log(1+i);d<-i/(1+i);
par(col="#6C7EBB",mai=c(0.25,.5,.5,.25),lwd=1,lty=1,pch="*");
layout(matrix(c(1,1,1:5,5,5),3,3,byrow=TRUE))

## Whole Life
    qd<-q[(x+1):101];
    Premium<-BLFOR(qd/(1+i),(1-qd)/(1+i),0)[1]/
                BLFOR(array(1,length(qd)),(1-qd)/(1+i),0)[1];
    V<-BLFOR(qd/(1+i)-Premium,(1-qd)/(1+i),0);
	# V is the vector of reserves
    plot((0:(100-x)),V,xlab=NA,ylab=NA,type="b",
            main=paste("Whole Life on (",x,")",sep=""));

## Endowment
    qd<-q[(x+1):(x+n)];
    Premium<-BLFOR(qd/(1+i),(1-qd)/(1+i),1)[1]/
                BLFOR(array(1,length(qd)),(1-qd)/(1+i),0)[1];
    V<-BLFOR(qd/(1+i)-Premium,(1-qd)/(1+i),1);

    MATV<-V;
    plot((0:(n-1)),V,xlab=NA,ylab=NA,type="b",
            main=paste(n," Yr.Endowment on (",x,")",sep=""));

## Term Insurance
    Premium<-BLFOR(qd/(1+i),(1-qd)/(1+i),0)[1]/
                BLFOR(array(1,length(qd)),(1-qd)/(1+i),0)[1];
    V<-BLFOR(qd/(1+i)-Premium,(1-qd)/(1+i),0);

    MATV<-cbind(MATV,V);
    plot((0:(n-1)),V,xlab=NA,ylab=NA,type="b",
            main=paste(n," Yr. Term on (",x,")",sep=""));

## Pure Endowment
    Premium<-BLFOR(array(0,length(qd)),(1-qd)/(1+i),1)[1]/
                BLFOR(array(1,length(qd)),(1-qd)/(1+i),0)[1];
    V<-BLFOR(array(-Premium,length(qd)),(1-qd)/(1+i),1);



    MATV<-cbind(MATV,V);
    plot((0:(n-1)),V,xlab=NA,ylab=NA,type="b",
            main=paste(n," Yr. Pure Endowment on (",x,")",sep=""));
    matplot((0:(n-1)),MATV,xlab=NA,ylab=NA,type="l",
            main=paste("Term/Endow./Pure Endow. - n=",n," on (",
                        x,")",sep=""));
    legend(x=0,y=0.8,col=blue,fill=c("black","red","green"),
        c("Endowment","Term","Pure Endowment"),horiz=FALSE)


tbl <- SOAtbl2df.agg(15006)
tbl
attributes(tbl)
q<-unlist(tbl$qx);
x<-30;n<-30;i<-.04;delta<-log(1+i);d<-i/(1+i);
## Whole Life
    qd<-q[(x+1):101];
    Premium<-BLFOR(qd/(1+i),(1-qd)/(1+i),0)[1]/
                BLFOR(array(1,length(qd)),(1-qd)/(1+i),0)[1];
    V<-BLFOR(qd/(1+i)-Premium,(1-qd)/(1+i),0);
	# V is the vector of reserves
	length(qd)


x <- 30
i <- .04
tbl <- SOAtbl2df.agg(15006)
tables <- list(mortality=tbl)
Premium=NA

Premium.WL <- function(x, i, tables) {
	out <- NULL
	mort.tbl <- tables$mortality
	if(x>attributes(mort.tbl)$omega) {
		stop("x is greater than highest age in the mortality table")
	}
	v <- 1/(1+i)
	qd<-unlist(mort.tbl[ which(mort.tbl$x >= x) , ]$qx);
	foo <-BLFOR(qd*v,(1-qd)*v,0)[1]/
            BLFOR(array(1,length(qd)),(1-qd)*v,0)[1];
	out <- data.frame(x=x, Premium=foo)
	return(out)
}

V.WL <- function(x, i, tables, Premium=NA) {
	out <- NULL
	mort.tbl <- tables$mortality
	if(x>attributes(mort.tbl)$omega) {
		stop("x is greater than highest age in the mortality table")
	}
	v <- 1/(1+i)
	qd<-unlist(mort.tbl[ which(mort.tbl$x >= x) , ]$qx);
	# age<-unlist(mort.tbl[ which(mort.tbl$x >= x) , ]$x);
	# policy.duration <- 0:(attributes(mort.tbl)$omega-x)
	if(is.na(Premium)) {
		Premium <- as.numeric(Premium.WL(x, i, tables)$Premium)
	}
	V<-BLFOR(qd*v - Premium,(1-qd)*v,0);
	# out <- data.frame(age, policy.duration, qd, V)
	out<- data.frame(V=V[1])
	return(out)
}

V.WL(x=30, i=.04, tables=list(mortality=tbl))

library(plyr)

tbl <- SOAtbl2df.agg(15006)
ages <- seq(25, 50, 5)
i.rates <- seq(.01, .05, .01)
mytbls <- list(mortality=tbl)
mptsdf <- data.frame(x=ages, i=.04, mort.table="tbl")
mptsdf
as.numeric(mptsdf[1,]$x)
age.prem <- adply(mptsdf, 1, .fun= function(w) Premium.WL(x=as.numeric(w$x), i=as.numeric(w$i), tables=list(mortality=eval(as.name(as.character(w$mort.table))))), .inform=TRUE)
age.prem
V_at_a_time <- function(mod.pts, ) {}
	
call("expression","print(tbl)")
# eval(as.name("tbl"))
