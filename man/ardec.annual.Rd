\name{ardec.annual}
\alias{ardec.annual}
\alias{ardec.annual.bayes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Estimation of the annual component from a monthly time series }
\description{
 Function ardec.annual extracts the annual component from the autoregressive decomposition of a monthly time series.

Function ardec.annual.bayes extracts the annual component from each autoregressive decomposition based on a simulated vector of autoregressive parameters.
}
\usage{
ardec.annual(x)

ardec.annual.bayes(x, R)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{time series}
  \item{R}{size of sample to be simulated from posterior}
}

\value{
  A list with components: 
  
  \item{period }{period for the anual component}
  \item{modulus }{damping factor for the annual component}
  \item{annual}{annual component (for ardec.annual)}
  \item{annualSim}{ matrix containing the simulated annual components as columns (for ardec.annual.bayes) }

}

\author{S. M. Barbosa}

\examples{
# warning: running the next command can be time comsuming!

data(tempEng)
ardec.annual(tempEng)
ardec.annual.bayes(tempEng,2)

}
\keyword{ts}% at least one, from doc/KEYWORDS
