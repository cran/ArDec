`ardec.lm` <-
function(x, method="burg",na.action=na.contiguous) {

require(stats)

if(any(is.na(x))) { warning("NA values in x - using ardec can give unreliable results!", call. = FALSE) }

 dat=x-mean(x,na.rm=TRUE)
 ndat=length(dat)

 p=ar(dat,method=method,na.action=na.action)[[1]] # p=order of autoregressive model from AIC (burg method)

 # linear autoregressive model fit

 X=t(matrix(dat[rev(rep((1:p),ndat-p)+ rep((0:(ndat-p-1)),rep(p,ndat-p)))],p,ndat-p))
 y=rev(dat[(p+1):ndat])
 fit=lm(y~-1+X, x=TRUE)

 return(fit)
 }

