########################################
## The algorithm to estimate the SLEM ##
########################################

#Perform the shuffle walk
shuffleWalk=function(A,u,M,T){

  samples=u
  X=u
  for (i in 1:T){

      s=sample(1:ncol(M),1)
      t=sample(c(1,-1),1)

      Xproposal=X+t*M[,s]

      if (all(Xproposal>=0)){

        X=Xproposal
      } #close if statement
     samples=rbind(samples,X)
   }#close for loop
return(samples)
}
