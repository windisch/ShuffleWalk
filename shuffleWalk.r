########################################
####### Mixed Shuffle Walk##############
########################################


require(ggplot2)

#Intial table (All columns need to have the same sum.)
u=matrix(c(8,8,3,
           3,5,8,
           5,3,5),nrow=3,ncol=3,byrow=T)

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

convPlot=function(df) {

#print last 10 percent of steps
n=round(0.10*nrow(df))
x=1:n
df=tail(df,n)

g <- ggplot(df, aes(x))
g <- g + ggtitle("Convergence plot")

fet=fisher.test(u)$p.value
g <- g + geom_hline(aes(yintercept=fet,lty="Fisher exact test"),show.legend=TRUE) 

ch2=chisq.test(u)$p.value
g <- g + geom_hline(aes(yintercept=ch2,lty="Chi2 test"),show.legend=TRUE) 

g <- g + labs(x="Steps",y="p-value") 
g <- g + geom_point(aes(y=df$MixedShuffle,colour="MixedShuffle"))
g <- g + geom_point(aes(y=df$Diaconis,colour="Diaconis"))

#remove legend
g <- g + theme(legend.title=element_blank())

g
}

#One step of Mixed Shuffle Walk
MixedShuffleStep=function(u){
    switch(sample(c("D","S"),size=1,prob=c(0.5,0.5)),
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
pValue=function(u,TT){
  
  Xu=chi2indep(u)
  cmethods <-c("Diaconis","MixedShuffle")
  df <- data.frame(Diaconis=double(TT),MixedShuffle=double(TT))

for(method in cmethods) {
  X<-c(1)
  for (i in 1:TT){
    
#### Decide which sampling method to use #####
    if(method=="MixedShuffle"){
        v=MixedShuffleStep(u) 
        }
    if(method=="Diaconis") {
        v=DiaconisStep(u)
        }
##############################################

    #metropolis rejection step 
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
   df[,method][i]=mean(X) 

  }
}

  return(df)
}
