\name{ardec.components}
\alias{ardec.components}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Autoregressive components }
\description{
  Extraction of autoregressive components from object of class "ardec"
}
\usage{
ardec.components(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{object of class "ardec" }
  \item{\dots}{ additional arguments for specific methods }
}
\value{
 A list with components
  \item{periodcomps }{periodic autoregressive components}
  \item{trendcomp}{trend component}
}
%\references{ ~put references to the literature/web site here ~ }
\author{ S. M. Barbosa }

%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
data(tempEng)
dec=ardec(tempEng,ardec.lm(tempEng)$coefficients)
comps=ardec.components(dec)
plot(comps$trendcomp)
plot(comps$periodcomps)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts }% __ONLY ONE__ keyword per line
