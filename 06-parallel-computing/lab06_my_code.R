#PHS 7045 lab 06




# 1 -----------------------------------------------------------------------

#write the outline of HW1 solutions. 
#We will do so first by understanding how this is working. We will use debug function.

#import from "HW1-solutions"

source("06-parallel-computing/HW1-solution.R")

debugonce(design1)
design1(y,h0=h0,h1=h1,h2=h2,h3=h3)




# 2 -----------------------------------------------------------------------

#If we want to use the same y_i's for design 1 and 2 
sim<-function(){
  y0 <- rbinom(N,size=1,prob=h0)
  y1 <- rbinom(N,size=1,prob=h1)
  y2 <- rbinom(N,size=1,prob=h2)
  y3 <- rbinom(N,size=1,prob=h3)
  
  y <- cbind("0"=y0,"1"=y1,"2"=y2,"3"=y3)
  
  d1<-design1(y,h0=h0,h1=h1,h2=h2,h3=h3)
  d2<-design2(y,N=N,nInterim = 40,h0=h0,h1=h1,h2=h2,h3=h3)
  list(d1,d2)
}




# parallel vs no parallel -------------------------------------------------

#run 1000 of these scenarios

design1(y,h0=h0,h1=h1,h2=h2,h3=h3)

library(parallel)
cores <- parallel::detectCores()
cl <- makePSOCKcluster(cores)

# 2. PREPARING THE CLUSTER
parallel::clusterSetRNGStream(cl, 123) # Equivalent to `set.seed(123)`
parallel::clusterExport(cl, list("N","y","h0","h1","h2","h3","design1","postDraws","design2"))

# 3. DO YOUR CALL
parallel.out.d1<-parallel::parSapply(cl, 1:1000, design1,y=y,h0=h0,h1=h1,h2=h2,h3=h3)
# parallel.out.d2<-parallel::parSapply(cl, 1:1000, design2, y=y, N=N, nInterim = 40, h0=h0, h1=h1 ,h2=h2, h3=h3) ##DESIGN2 doesn't work 

#is that output same as the serial?
serial.out.d1<-sapply(1:1000, design1,y=y,h0=h0,h1=h1,h2=h2,h3=h3)
# serial.out.d2<-sapply(1:1000, design2, y=y, N=N, nInterim = 40, h0=h0, h1=h1 ,h2=h2, h3=h3)
# replicate(10, design2(y,N=N,nInterim = 40,h0=h0,h1=h1,h2=h2,h3=h3))

#compare with and without the parallel

#for design 1
library(bench)
mark(
  parallel = parallel::parSapply(cl, 1:1000, design1,y=y,h0=h0,h1=h1,h2=h2,h3=h3),
  serial = sapply(1:1000, design1,y=y,h0=h0,h1=h1,h2=h2,h3=h3),
  relative=TRUE, check=FALSE
)
#serial is slower!


# #for design 2
# mark(
#   parallel = parallel::parSapply(cl, 1:1000,design2,y=y,N=N,nInterim = 40,h0=h0,h1=h1,h2=h2,h3=h3),
#   serial = sapply(1:1000, design2,y=y,N=N,nInterim = 40,h0=h0,h1=h1,h2=h2,h3=h3),
#   relative=TRUE, check=FALSE
# )

