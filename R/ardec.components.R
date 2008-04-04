`ardec.components` <-
function(object, ...) {

setClass("ardec",representation(start="numeric",frequency="numeric",period="numeric",modulus="numeric",comps="matrix"),package="ArDec")

periodcomps=list(periods=abs(object@period[which(which(object@modulus>=0.95)%%2==0)]),comps=ts(t(2*Re(object@comps[which(which(object@modulus>=0.95)%%2==0),])),start=object@start,frequency=object@frequency) )

trendcomps=ts(Re(object@comps[which(object@period==Inf)[1],]),start=object@start,frequency=object@frequency) 
 
 return(list(periodcomps=periodcomps,trendcomp=trendcomps))
 }

