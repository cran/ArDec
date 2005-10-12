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
        comp = ardec(x, fit$coefficients)
        if (any(comp$period > 10 & comp$period < 14)) {
            l[i] = comp$period[which(comp$period > 10 & comp$period < 
                14)]
            m[i] = comp$modulus[which(comp$period > 10 & comp$period < 
                14)]
            gt[i, ] = Re(comp$comps[which(comp$period > 10 & 
                comp$period < 14), ] + comp$comps[which(comp$period > 
                10 & comp$period < 14) + 1, ])
            i = i + 1
        }
    }
    return(list(period = l, modulus = m, annualSim = gt))
}
