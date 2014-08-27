
fun1 <- function(x, y) {
	fun2 <- function(...) {
		cat("x is ",x,", and y is ",y,"\n",sep="")
	}
	return(fun2())
}
fun1(2,3)