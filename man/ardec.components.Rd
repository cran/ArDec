\name{ardec.components}
\alias{ardec.components}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to extract autoregressive components }
\description{
  Extraction of autoregressive components from object of class "ardec"
}
\usage{
ardec.components(object, th = 0.95, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{object of class "ardec" }
  \item{th}{value of damping threshold}
  \item{\dots}{ additional arguments for specific methods }
}
\value{
 A list with components
  \item{periodcomps }{periodic components}
  \item{trendcomp}{trend component}
}
%\references{}

\author{ S. M. Barbosa }

%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
data(tempEng)
decomposition=ardec(tempEng,ardec.lm(tempEng)$coefficients)
components=ardec.components(decomposition)
plot(components$trendcomp)
plot(components$periodcomps$comps)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts }% __ONLY ONE__ keyword per line
