########################################
####### Mixed Shuffle Walk##############
########################################

#Intial table (All columns need to have the same sum.)
u=matrix(c(3,5,3,
           2,7,7,
           8,1,3),nrow=3,ncol=3,byrow=T)

#How many steps?
TT=100

#One step of diaconis Walk
DiaconisStep=function(u){
  X=u
  r=dim(u)[1]
  c=dim(u)[2]
  rr=sample(1:r,size=2,replace = F)
  cc=sample(1:c,size=2,replace = F)
  t=sample(c(-1,1),size=1)
  X[rr[1],cc[1]]=X[rr[1],cc[1]]+t
  X[rr[2],cc[2]]=X[rr[2],cc[2]]+t
  X[rr[1],cc[2]]=X[rr[1],cc[2]]-t
  X[rr[2],cc[1]]=X[rr[2],cc[1]]-t
  
  if (all(X>=0)){
            u=X
          }
return(u)
  } 

#One step of Shuffle Walk
ShuffleStep=function(u){
  c=dim(u)[2]
  cc=sample(1:c,size=2,replace=F)
  c1=u[,cc[1]]
  c2=u[,cc[2]]
  u[,cc[1]]=c2
  u[,cc[2]]=c1
  return(u)     
}

#One step of Mixed Shuffle Walk
MixedShuffleStep=function(u){
    switch(sample(c("D","S"),size=1),
        "D"={u=DiaconisStep(u)},
        "S"={u=ShuffleStep(u)})
    return(u)
    }

#compute the rejection probability of MH-Algorihm
metropolisRejection=function(u,v){
#current state: u
#new state: v
    u=as.vector(u)
    v=as.vector(v)
    d=length(u)
    p=1
    for(i in 1:d){
        if(u[i]!=v[i]){
            p=p*factorial(u[i])/factorial(v[i])
        }
    }
    return(1-min(1,p))
}

#Chi2 for independence
chi2indep=function(u){
    rS=rowSums(u)
    r=length(rS)
    cS=colSums(u)
    c=length(cS)
    n=sum(u)
    X=0
    for(i in 1:r){
        for(j in 1:c){
            MLE=rS[i]*cS[j]/n
            X=X+(u[i,j]-MLE)^2/MLE
            }
        }
    return(X)
}

#Estimate the p-value
pValue=function(u,TT,method){
  
  Xu=chi2indep(u)
  X=c(1)
  M=c(1)

  if(missing(method)) {
      method="MixedShuffle";    
  }
  
  for (i in 2:TT){
    
#### Decide which sampling method to use #####
    if(method=="MixedShuffle"){
        v=MixedShuffleStep(u) 
        } else {
        v=DiaconisStep(u)
        }
##############################################

    #rejection probability
    q=metropolisRejection(u,v)
    if(sample(c(0,1),size=1,prob=c(q,1-q))==1){
        #do not reject
        u=v
        }
    if(chi2indep(u)>=Xu){
         X=c(X,1)
    } else {
         X=c(X,0)
        }
    M=c(M,mean(X))
  }
  return(M)
}
