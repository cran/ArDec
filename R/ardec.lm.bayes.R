`ardec.lm.bayes` <-
function(x, R, med=TRUE){

#require(ardec.sampling)

fit=ardec.lm(x)

p=length(fit$coefficients)
phi=matrix(nrow=p,ncol=R)


i=1

set.seed(1)

while(i<=R){
           phi[,i]=as.vector(ardec.sampling(x,fit)$ARcoef)  
           i=i+1
           }

beta=apply(phi,1,median)

if(med==TRUE){return(list(BayesCoef=beta))}

return(list(BayesCoef=phi)) 
}

