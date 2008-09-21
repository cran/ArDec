`ardec` <-
function (x, coef, th = 0.95, ...) 
{
    setClass("ardec", representation(start = "numeric", frequency = "numeric", 
        period = "numeric", modulus = "numeric", comps = "matrix"), 
        package = "ArDec")
    setMethod("show", "ardec", function(object) {
        out = cbind(unique(abs(object@period[which(object@modulus >= 
            th)])), unique(object@modulus[which(object@modulus >= 
            th)]))
        if (any(object@period == Inf)) {
            out[which(out == Inf), 1] = c("trend")
        }
        if (all(object@period != Inf)) {
            out = rbind(out, c("no trend", ""))
        }
        dimnames(out) = list(c(1:(nrow(out))), c("period", "damping"))
        show(out)
    })
    if (any(is.na(x))) {
        warning("NA values in x - using ardec can give unreliable results!", 
            call. = FALSE)
    }
    dat = x - mean(x, na.rm = TRUE)
    p = length(coef)
    ndat = length(x)
    G = matrix(nrow = p, ncol = p)
    G[1, ] = coef
    G[seq(2, p), seq(1, p - 1)] = diag(1, (p - 1))
    G[seq(2, p), p] = 0
    modulus = Mod(eigen(G)[[1]])
    lambda = 2 * pi/Arg(eigen(G)[[1]])
    eigenvalues = eigen(G)
    A = diag(eigenvalues[[1]])
    E = eigenvalues[[2]]
    B = solve(E)
    F = rep(NA, p)
    F[1] = 1
    F[seq(2, p)] = 0
    a = t(E) %*% F
    d = diag(as.vector(a))
    H = d %*% B
    Z = matrix(nrow = p, ncol = ndat)
    Z[1, ] = dat
    for (i in seq(1, p - 1)) {
        Z[i + 1, ] = as.vector(filter(x = dat, filter = c(rep(0, 
            i), 1), method = "convolution", sides = 1))
    }
    g = matrix(nrow = p, ncol = ndat)
    for (j in seq(1, p)) {
        for (t in seq(1, ndat)) {
            g[j, t] = H[j, ] %*% Z[, t]
        }
    }
    new("ardec", start = start(x), frequency = frequency(x), 
        period = lambda, modulus = modulus, comps = g)
}
