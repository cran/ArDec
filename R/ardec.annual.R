"ardec.annual" <-
function (x) 
{
    if (frequency(x) != 12) {
        stop("monthly time series required")
    }
    fit = ardec.lm(x)
    comp = ardec(x, fit$coefficients)
    if (any(comp$period > 10 & comp$period < 14)) {
        l = comp$period[which(comp$period > 10 & comp$period < 
            14)]
        m = comp$modulus[which(comp$period > 10 & comp$period < 
            14)]
        gt = Re(comp$comps[which(comp$period > 10 & comp$period < 
            14), ] + comp$comps[which(comp$period > 10 & comp$period < 
            14) + 1, ])
    }
    return(list(period = l, modulus = m, annual = gt))
}
