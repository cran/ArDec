"ardec.periodic.bayes" <-
function (x, per, R, tol = 1) 
{
    l = seq(1, R, by = 1)
    m = seq(1, R, by = 1)
    gt = matrix(nrow = R, ncol = length(x))
    if (frequency(x) != 12) {
        stop("monthly time series required")
    }
    fit = ardec.lm(x)
    i = 1
    set.seed(1)
    while (i <= R) {
        phi = as.vector(ardec.sampling(x, fit)$ARcoef)
        comp = ardec(x, phi)
        if (any(comp$period > (per - tol) & comp$period < (per + 
            tol))) {
            candidates = which(comp$period > (per - tol) & comp$period < 
                (per + tol))
            l[i] = comp$period[candidates][which.max(candidates)]
            m[i] = max(comp$modulus[candidates])
            gt[i, ] = Re(comp$comps[candidates[which.max(candidates)], 
                ] + comp$comps[candidates[which.max(candidates)] + 
                1, ])
            i = i + 1
        }
    }
    return(list(period = l, modulus = m, compSim = gt))
}
