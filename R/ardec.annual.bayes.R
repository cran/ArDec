"ardec.annual.bayes" <-
function (x, R) 
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
        if (any(comp$period > 11 & comp$period < 13)) {
            candidates = which(comp$period > 11 & comp$period < 
                13)
            l[i] = comp$period[candidates][which.max(candidates)]
            m[i] = max(comp$modulus[candidates])
            gt[i, ] = Re(comp$comps[candidates[which.max(candidates)], 
                ] + comp$comps[candidates[which.max(candidates)] + 
                1, ])
            i = i + 1
        }
    }
    return(list(period = l, modulus = m, annualSim = gt))
}
