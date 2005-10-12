"ardec.periodic" <-
function (x, per, tol = 1) 
{
    if (frequency(x) != 12) {
        stop("monthly time series required")
    }
    fit = ardec.lm(x)
    comp = ardec(x, fit$coefficients)
    if (any(comp$period > (per - tol) & comp$period < (per + 
        tol))) {
        candidates = which(comp$period > (per - tol) & comp$period < 
            (per + tol))
        l = comp$period[candidates][which.max(candidates)]
        m = max(comp$modulus[candidates])
        gt = Re(comp$comps[candidates[which.max(candidates)], 
            ] + comp$comps[candidates[which.max(candidates)] + 
            1, ])
    }
    return(list(period = l, modulus = m, component = gt))
}
