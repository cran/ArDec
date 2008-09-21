`ardec.lm.bayes` <-
function (x, method = "burg", na.action = na.contiguous, R, med = TRUE, 
    seed = 1) 
{
    if (any(is.na(x))) {
        warning("NA values in x - using ardec can give unreliable results!", 
            call. = FALSE)
    }
    fit = ardec.lm(x, method = method, na.action = na.action)
    p = length(fit$coefficients)
    phi = matrix(nrow = p, ncol = R)
    i = 1
    set.seed(seed)
    while (i <= R) {
        phi[, i] = as.vector(ardec.sampling(x, fit)$ARcoef)
        i = i + 1
    }
    beta = apply(phi, 1, median)
    if (med == TRUE) {
        return(list(coefficients = beta))
    }
    return(list(coefficients = phi))
}
