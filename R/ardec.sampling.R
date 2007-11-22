`ardec.sampling` <-
function (x, fit) 
{
    require(stats)
    p = length(fit$coefficients)
    a = solve(t(fit$x) %*% fit$x)
    a = a + t(a)/2
    v = 1/(rgamma(1, fit$df.residual/2) * 2/sum(fit$residuals^2))
    coef = fit$coefficients + (t(chol(v * a)) %*% (rnorm(p)))
    return(list(ARcoef = coef))
}
