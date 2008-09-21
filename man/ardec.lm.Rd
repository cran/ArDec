\name{ardec.lm}
\alias{ardec.lm}
\alias{ardec.lm.bayes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to fit an autoregressive model as a linear regression }
\description{
  Function ardec.lm fits an autoregressive model of order p, AR(p) to a time series through a linear least squares regression.

Function ardec.lm.bayes provides a sample of autoregressive parameters from the multivariate normal posterior distribution for the coefficients assuming a (non-informative) reference prior.
}
\usage{
ardec.lm(x, method = "burg", na.action = na.contiguous)

ardec.lm.bayes(x, method = "burg", na.action = na.contiguous, R, med = TRUE, seed = 1)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ time series }
  \item{method}{ method used to fit the model ("yule-walker", "burg", "ols", "mle", "yw"); defaults to "burg" }
  \item{na.action}{   function to be called to handle missing values; defaults to na.contiguous}
  \item{R}{size of sample to be simulated from posterior}
  \item{med}{logical, indicating if a median vector of autoregressive parameters should be computed from the simulated sample}
  \item{seed}{integer for set.seed}
}

\value{

  For ardec.lm, an object of class "lm".

  For ardec.lm.bayes an Rxp matrix containing the samples of autoregressive coefficients as columns (if med=FALSE).

 If med=TRUE, ardec.lm.bayes returns  a single column matrix containing the median vector of autoregressive parameters.
}
%\references{}

\author{S. M. Barbosa}

\seealso{ \code{\link{ar}}, \code{\link{lm}} }
\examples{
data(tempEng)
model=ardec.lm(tempEng)
}
\keyword{ ts }% at least one, from doc/KEYWORDS
