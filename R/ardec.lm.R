"ardec.lm" <-
function (x) 
{
    dat = x - mean(x)
    ndat = length(dat)
    p = ar(dat, method = "burg")[[1]]
    X = t(matrix(dat[rev(rep((1:p), ndat - p) + rep((0:(ndat - 
        p - 1)), rep(p, ndat - p)))], p, ndat - p))
    y = rev(dat[(p + 1):ndat])
    fit = lm(y ~ -1 + X, x = TRUE)
    return(fit)
}
