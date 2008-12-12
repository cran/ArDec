`ardec.components` <-
function(object, th=0.95, ...) {

setClass("ardec",representation(start="numeric",frequency="numeric",period="numeric",modulus="numeric",comps="matrix"),package="ArDec")


periodcomps=list(periods=abs(object@period[which(object@modulus>=th & object@period !=Inf)[seq(1,length(which(object@modulus>=th & object@period !=Inf)),by=2)]]),comps=ts(2*Re(t(object@comps)[,which(object@modulus>=th & object@period !=Inf)[seq(1,length(which(object@modulus>=th & object@period !=Inf)),by=2)]]),start=object@start,frequency=object@frequency) )


if(any(object@period==Inf)){
trendcomps=ts(Re(object@comps[which(object@period==Inf)[1],]),start=object@start,frequency=object@frequency)
}

if(all(object@period!=Inf)) {trendcomps=NULL}

 return(list(periodcomps=periodcomps,trendcomp=trendcomps))
 }

