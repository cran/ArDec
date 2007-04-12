`ardec.trend.bayes` <-
function(x, R){

options(warn=-1)

l=seq(1,R,by=1)
m=seq(1,R,by=1)
gt=matrix(nrow=R,ncol=length(x))


fit=ardec.lm(x)

i=1

set.seed(1)

while(i<=R){
         phi=as.vector(ardec.sampling(x,fit)$ARcoef)
         comp=ardec(x,phi)

         
        if (any(comp$period==Inf)){warning("no trend component")}

         if(any(comp$period ==Inf)){
         l[i]=comp$period[which(match(comp$period,Inf)==1)[1]]
         m[i]=comp$modulus[which(match(comp$period,Inf)==1)[1]]
         gt[i,]=Re(comp$comps[which(match(comp$period,Inf)==1 )[1],])
         i=i+1} 

}


return(list(modulus=m,trendSim=gt))  


}

