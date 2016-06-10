########################################
## The algorithm to estimate the SLEM ##
########################################

#Example

A=matrix(c(1,1,1,0,0,0,
           0,0,0,1,1,1,
           1,0,0,1,0,0,
           0,1,0,0,1,0,
           0,0,1,0,0,1),nrow=5,ncol=6,byrow=T)
M=markov(A)
u=c(2,0,1,0,2,1)
#number of rows
r=3
#number of columns
c=2

#Perform the shuffle walk
BasicMove=function(A,u,M,TT){

  samples=u
  X=u
  #for (i in 1:TT){

      s=sample(1:ncol(M),1)
      t=sample(c(1,-1),1)

      Xproposal=X+t*M[,s]

      if (all(Xproposal>=0)){

        X=Xproposal
      } #close if statement
     samples=rbind(samples,X)
#   }#close for loop
return(X)
}

ShuffleWalk=function(u,r,c){
  u=matrix(u,nrow=r,ncol=c,byrow=F)
  cc=sample(1:c,size=2,replace=F)
  c1=u[,cc[1]]
  c2=u[,cc[2]]
  u[,cc[1]]=c2
  u[,cc[2]]=c1
}

MixShuffle=function(){
  #######
}