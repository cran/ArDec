\name{ardec.sampling}
\alias{ardec.sampling}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Auxiliary function }
\description{
 Auxiliary function: sampling from a linear model posterior distribution assuming a reference (non-informative) prior
}
\usage{
ardec.sampling(x, fit)
}

\arguments{
  \item{x}{ time series }
  \item{fit}{object of class "lm" from a linear regression fit}
}

\value{
  \item{ARcoef}{vector of autoregressive parameters sampled from the posterior distribution}}

\author{S. M. Barbosa}

\examples{
data(tempEng)
model=ardec.lm(tempEng)
ardec.sampling(tempEng,model)

}
\keyword{ ts }% at least one, from doc/KEYWORDS
