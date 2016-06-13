########################################
####### Mixed Shuffle Walk##############
########################################

#Intial table (All columns need to have the same sum.)
u=matrix(c(1,1,1,
           1,1,1,
           1,1,1),nrow=3,ncol=3,byrow=T)

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


#Mixed Shuffle Walk (print all TT tables obseved)
MixedShuffle=function(u,TT){
  
  tables=list()
  tables[[1]]=u
  
  for (i in 1:TT){
    
    walk=sample(c("D","S"),size=1)
    switch(sample(c("D","S"),size=1),
        "D"={u=DiaconisStep(u)},
        "S"={u=ShuffleStep(u)},
            {print('default')})
    
    tables[[(i+1)]]=u
  }
  return(tables)
}
