library(lifecontingencies)
library(rbenchmark)
data("soa08Act")

i <- 0.1

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

soa08Act

method1 <- function( n )
{
	for (j in 1:n) {
		Axn(soa08Act, x=0,i=i)
	}
}

# A little algebra to get the input for method2
# It uses q's rather than p's of the lifetable
p0 <- soa08Act@lx/soa08Act@lx[1]
q <- (p0[1:110] - p0[2:111])/p0[1:110]

method2 <- function( n )
{
	for (j in 1:n) {
		BLFOR(q/(1+i),(1-q)/(1+i),1)
	}
}

# Let's check that they get the same answer
A1 <- Axn(soa08Act, x=0,i=i)
A2 <- BLFOR(q/1.1,(1-q)/1.1,1)
A1
A2
A2[1]
A1 - A2[1]
# There is some floating point error going on, but they more or less get to the same answer.

# Now for the first test:
n <- 100

res <- benchmark(method1(n),
                 method2(n),
                 order="relative",
                 replications = 5)
print(res[,1:4])
# There should be a significant improvement to using BLFOR.

# Let's try to write BLFOR in C++
library(inline)

# This would need to get cleaned up, but basically works
# You'd want to write it a a stand-alone (compiled?) function
src <- '
Rcpp::NumericVector cr(r);
Rcpp::NumericVector cs(s);
Rcpp::NumericVector out(Rcpp::clone(r));
double temp = Rcpp::as<double>(bv);
for (int i=cr.size()-1; i>=0; --i) {
        out[i] = cr[i] + cs[i]*temp;
        temp = out[i];
    }
return Rcpp::wrap(out);'
BLFORRcpp <- cxxfunction(signature(r="numeric", s="numeric", bv="numeric"), src, plugin="Rcpp")

# Let's check that they get the same answer
BLFORRcpp(q/1.1,(1-q)/1.1,1)
A2
prod(BLFORRcpp(q/1.1,(1-q)/1.1,1)==A2)
# It works

method3 <- function( n )
{
	for (j in 1:n) {
		BLFORRcpp(q/1.1,(1-q)/1.1,1)
	}
}

n <- 100

res <- benchmark(method1(n),
                 method2(n),
                 method3(n),
                 order="relative",
                 replications = 5)
print(res[,1:4])
# ... So, it is *a lot* faster in c++ than in the lifecontingencies package

# TODO: parallel CPU implementation? GPU implementation?