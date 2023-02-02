#PHS 7045 Advanced Programming in R and CHPC
#Lab 02-essentials
#Hyejung Lee

rm(list=ls())




# equal allocation --------------------------------------------------------

#equal allocation doesn't have 40 things 
#equally distribute N=228 into 4 arms to begin with 
set.seed(1911)
Y0<-rbinom(N/4,1,0.35)
Y1<-rbinom(N/4,1,0.35)
Y2<-rbinom(N/4,1,0.35)
Y3<-rbinom(N/4,1,0.35)

#generate posterior samples 

#Control posterior
p0<-rbeta(1000, shape1 = 0.35 + sum(Y0), shape2 = 0.65 + 40 -sum(Y0))

#arm1 posterior
p1<-rbeta(1000, shape1 = 0.35 + sum(Y1), shape2 = 0.65 + 40 -sum(Y1))

#arm2 posterior
p2<-rbeta(1000, shape1 = 0.35 + sum(Y2), shape2 = 0.65 + 40 -sum(Y2))

#arm3 posterior
p3<-rbeta(1000, shape1 = 0.35 + sum(Y3), shape2 = 0.65 + 40 -sum(Y3))



#Calculate probability that each pi is greater than the control
all.probs<-c(mean(p1>p0),mean(p2>p0),mean(p3>p0))

#check if the maximum probability is greater than the delta threashold
max(all.probs)>0.9912 #FALSE
#The treatment failed.




# Response adaptive randomization -----------------------------------------


#generate function that completes one cycle of test given a sample size, patients (Y) and allocation probability.
one_cycle<-function(S, N, Y, count.patients, alloc.prob=NULL, trt.arm=NULL){
  #S = numeric. number of samples 
  #N = total number of patients remaining
  #count.patients = a vector of length 4, which counts number of patients allocated in each arm thus far, starting from arm 0,1,2, and 3. 
  #alloc.prob = a vector of allocation probability in each arm
  #trt.arm = a vector of treatment arm allocation for each sample. If null, we are doing RAR. If not null, it must be of length S, and have values of 0,1,2, or 3. It is meant for the initial round of RAR. 
  
  
  if(!is.null(trt.arm) & (length(trt.arm)!=S)){ #If we manually gave treatment assignment (trt.arm) but the sample number (S) is not equal to the treatment assignment quantity, then we stop.
    return(stop("The number of treatment arms should be same as the sample size. That is, length(trt.arm)==S should hold."))
  }
  
  if(is.null(trt.arm) & is.null(alloc.prob)){ #If we are cycling through RAR (not initial round) but didn't get the allocaiton probability,
    return(stop("allocation probability should be given for each arm."))
  }
  
  if(is.null(trt.arm)){
    #Sample the treatment arms using allocation probability
    trt.arm<-sample(x=0:3, size=S, replace = TRUE, prob=alloc.prob)
  }
  
  
  #Keep track of number of patients in each treatment arm.
  new.count.patients<-c(sum(trt.arm==0),sum(trt.arm==1),sum(trt.arm==2),sum(trt.arm==3))
  count.patients<-count.patients+new.count.patients
  names(count.patients)<-0:3
  
  #generate 1000 posterior samples 
  post.samples<-matrix(NA,ncol = 4, nrow=1000)
  #Control posterior
  post.samples[,1]<-rbeta(1000, shape1 = 0.35 + sum(Y[which(trt.arm==0)]), shape2 = 0.65 + S -sum(Y[which(trt.arm==0)]))
  
  #arm1 posterior
  post.samples[,2]<-rbeta(1000, shape1 = 0.35 + sum(Y[which(trt.arm==1)]), shape2 = 0.65 + S -sum(Y[which(trt.arm==1)]))
  
  #arm2 posterior
  post.samples[,3]<-rbeta(1000, shape1 = 0.35 + sum(Y[which(trt.arm==2)]), shape2 = 0.65 + S -sum(Y[which(trt.arm==2)]))
  
  #arm3 posterior
  post.samples[,4]<-rbeta(1000, shape1 = 0.35 + sum(Y[which(trt.arm==3)]), shape2 = 0.65 + S -sum(Y[which(trt.arm==3)]))
  
  
  #calculate allocation probability
  success.arms<-apply(post.samples,1,function(x){which.max(x)}) #count number of times each arm was the maximum
  success.arms<- success.arms-1 #our arm count starts at 0=control, so we subtract by 1
  
  alloc.prob<-c(sum(success.arms==0),sum(success.arms==1),sum(success.arms==2),sum(success.arms==3))/1000 #for arms 1,2, and 3, it's jut proportion that the posterior samples were the maximum. 
  #allocation profitability of control arm:
  n_t<-sum(count.patients[c("1","2","3")])
  n_0<-count.patients["0"]
  alloc.prob[1]<-min(sum(alloc.prob[2:4]*((n_t+1)/(n_0+1))), max(alloc.prob[2:4]))
  
  
  #update remaining sample number 
  N<- N-S
  
  #Calculate probability that each pi is greater than the control
  all.probs<-c(mean(post.samples[,2]>post.samples[,1]),
               mean(post.samples[,3]>post.samples[,1]),
               mean(post.samples[,4]>post.samples[,1]))
  #return maximum of all of these probaiblity to decide whether the study was successful or not
  
  
  
  
  #return remaining people, number of people assigned to each arm, allocation probability, and maximum probability of all probabiilties for each arm being greater than control.
  list("N"=N,
       "count.patients"=count.patients,
       "alloc.prob"=alloc.prob,
       "max.prob.trt.arm"=max(all.probs)
  )
}


#create a function that performs RAR given total number of patients N, where interim analysis occurs for every 40th patients. 
RAR<-function(N, S=40){
  
  #success rate = 0.35
  
  
  #first round of 40 patients is equal allocation.
  output<-one_cycle(S=S,N=228,Y=rbinom(40,1,0.35), trt.arm = rep(0:3,each=10), count.patients = rep(0,4))
  
  #update total number of patients
  N<-output$N
  

  #go through cycle per 40 patients at a time
  while(N>=S){
    
    output<-one_cycle(S=S,N=N,Y=rbinom(40,1,0.35), alloc.prob = output$alloc.prob, count.patients = output$count.patients)
    N<-output$N
  }
  
  if(N>0){ #If we still have remaining people, complete the cycle once more
    output<-one_cycle(S=N,N=N,Y=rbinom(40,1,0.35), alloc.prob = output$alloc.prob, count.patients = output$count.patients)
    N<-output$N
  }
  
  list("max.prob.trt.arm"=output$max.prob.trt.arm,
       "count.patients"=output$count.patients)
  
  
}


#test my code
set.seed(101)
check.RAR<-RAR(N=228)
check.RAR$max.prob.trt.arm #The probability that the best treatment arm is better than control.
check.RAR$count.patients #The number of patients assigned to each treatment arm.
sum(check.RAR$count.patients) #should sum up to 228
