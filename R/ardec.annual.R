"ardec.annual" <-
function (x) 
{
    if (frequency(x) != 12) {
        stop("monthly time series required")
    }
    fit = ardec.lm(x)
    comp = ardec(x, fit$coefficients)
    if (any(comp$period > 11 & comp$period < 13)) {
        candidates = which(comp$period > 11 & comp$period < 13)
        l = comp$period[candidates][which.max(candidates)]
        m = max(comp$modulus[candidates])
        gt = Re(comp$comps[candidates[which.max(candidates)], 
            ] + comp$comps[candidates[which.max(candidates)] + 
            1, ])
    }
    return(list(period = l, modulus = m, annual = gt))
}
