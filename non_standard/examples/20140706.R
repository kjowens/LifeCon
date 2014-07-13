# https://stackoverflow.com/questions/5339796/loading-an-r-package-from-a-custom-directory
library(devtools)

# load package w/o installing
load_all('/Volumes/hd500/LifeCon/LifeCon')

# or invoke 'R CMD INSTALL'
# install('/some/package/diR')

rcpp_hello_world()