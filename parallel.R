## @knitr R-linalg

library(RhpcBLASctl)
x <- matrix(rnorm(5000^2), 5000)

blas_set_num_threads(4)
system.time({
   x <- crossprod(x)
   U <- chol(x)
})

##   user  system elapsed 
##  8.316   2.260   2.692 

blas_set_num_threads(1)
system.time({
   x <- crossprod(x)
   U <- chol(x)
})

##   user  system elapsed 
##  6.360   0.036   6.399 


## @knitr mcparallel

library(parallel)
n <- 10000000
system.time({
	p <- mcparallel(mean(rnorm(n)))
	q <- mcparallel(mean(rgamma(n, shape = 1)))
	s <- mcparallel(mean(rt(n, df = 3)))
	res <- mccollect(list(p,q, s))
})

system.time({
	p <- mean(rnorm(n))
	q <- mean(rgamma(n, shape = 1))
	s <- mean(rt(n, df = 3))
})


## @knitr foreach

library(doParallel)  # uses parallel package, a core R package

source('rf.R')  # loads in data and looFit()

looFit

nCores <- 4  
registerDoParallel(nCores) 

nSub <- 10  # do only first 10 for illustration

result <- foreach(i = 1:nSub) %dopar% {
	cat('Starting ', i, 'th job.\n', sep = '')
	output <- looFit(i, Y, X)
	cat('Finishing ', i, 'th job.\n', sep = '')
	output # this will become part of the out object
}
print(unlist(result[1:5]))

## @knitr parallel-lsApply

library(parallel)
nCores <- 4  
cl <- makeCluster(nCores) 

nSub <- 10
input <- seq_len(nSub) # same as 1:nSub but more robust

## clusterExport(cl, c('x', 'y')) # if the processes need objects
## from main R workspace (not needed here as no global vars used)

## need to load randomForest package within function
## when using par{L,S}apply, the last) argument being
## set to TRUE causes this to happen (see rf.R)
system.time(
	res <- parSapply(cl, input, looFit, Y, X, TRUE) 
)
system.time(
	res2 <- sapply(input, looFit, Y, X)
)

## @knitr parallel-mclapply-no-preschedule

library(parallel)

nCores <- 4

## specifically designed to be slow when have four cores
## and prescheduling as the slow tasks all assigned to one worker
n <- rep(c(1e7, 1e5, 1e5, 1e5), 4)


fun <- function(n) {
    cat("working on ", n, "\n")
    mean(rnorm(n))
}

system.time(
	res <- mclapply(n, fun, mc.cores = nCores) 
)
system.time(
	res <- mclapply(n, fun, mc.cores = nCores, mc.preschedule = FALSE) 
)



## @knitr doSNOW

library(doSNOW)

## Specify the machines you have access to and number of cores to use on each:
machines = c(rep("beren.berkeley.edu", 1),
    rep("gandalf.berkeley.edu", 1),
    rep("arwen.berkeley.edu", 2))

## On Savio and other clusters using the SLURM scheduler:
## machines <- system('srun hostname', intern = TRUE)

cl = makeCluster(machines, type = "SOCK")
cl

registerDoSNOW(cl)

fun = function(i, n = 1e6)
  out = mean(rnorm(n))

nTasks <- 120

print(system.time(out <- foreach(i = 1:nTasks) %dopar% {
	outSub <- fun(i)
	outSub # this will become part of the out object
}))

stopCluster(cl)  

## @knitr sockets

library(parallel)

machines = c(rep("beren.berkeley.edu", 1),
    rep("gandalf.berkeley.edu", 1),
    rep("arwen.berkeley.edu", 2))
cl = makeCluster(machines)
cl

n = 1e7
## copy global variable to workers
clusterExport(cl, c('n'))

fun = function(i)
  out = mean(rnorm(n))
  
result <- parSapply(cl, 1:20, fun)

result[1:5]

stopCluster(cl) 

## @knitr parallel-copy

library(parallel)

testfun <- function(i, data) {
    set.seed(i)
    n <- nrow(data)
    smp <- sample(n, n, replace = TRUE)
    ## do some silly calculation on the bootstrapped rows
    ## note that in all cases data[smp, ] causes a copy to be made
    return(mean(data[smp, ]))
}


data <- matrix(rnorm(1e8), nrow = 1e6)
object.size(data)

## in terminal monitor memory use with: watch -n 0.1 free -h

cl <- makeCluster(4)
parSapply(cl, 1:4, testfun, data)
stopCluster(cl)

## @knitr parallel-nocopy

testfun_global <- function(i) {
    set.seed(i)
    n <- nrow(data)  ## yes, use global variable
    smp <- sample(n, n, replace = TRUE)
    ## do some silly calculation on the bootstrapped rows
    ## note that in all cases data[smp, ] causes a copy to be made
    return(mean(data[smp, ]))
}
   
## in terminal monitor memory use with: watch -n 0.1 free -h

## Forked processes do not make copies of 'data'
cl <- makeCluster(4, type = "FORK")
parSapply(cl, 1:4, testfun_global)
stopCluster(cl)

## To contrast, we see what happens if not using
## a fork-based cluster.
## Note that this would also require exporting the global variable(s).

cl <- makeCluster(4)
parSapply(cl, 1:4, testfun_global)  ## error because 'data' not available on workers
clusterExport(cl, 'data')
parSapply(cl, 1:4, testfun_global)
stopCluster(cl)

## @knitr RNG-apply

library(parallel)
library(rlecuyer)

nSims <- 250
taskFun <- function(i){
	val <- runif(1)
	return(val)
}

nCores <- 4
RNGkind()
cl <- makeCluster(nCores)
iseed <- 1
clusterSetRNGStream(cl = cl, iseed = iseed)
RNGkind() # clusterSetRNGStream sets RNGkind as L'Ecuyer-CMRG
## but it doesn't show up here on the master
res <- parSapply(cl, 1:nSims, taskFun)
## now redo with same master seed to see results are the same
clusterSetRNGStream(cl = cl, iseed = iseed)
res2 <- parSapply(cl, 1:nSims, taskFun)
identical(res,res2)
stopCluster(cl)



