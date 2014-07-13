# 2014-07-06, KJO: small untility functions

# eval(parse(text="tbl")) # this is how you evaluate code as a string
.evaltxt <- function(txt) {
	# evaluates arbitrary code as text, could be dangerous
	# useful when you need to iterate over a list of strings
	if(is.character(txt)){
		eval(parse(text=txt))
	}
}
