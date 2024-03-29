\name{calcGM}
\alias{calcGM}
\title{Calculate the Geometric Mean, Allowing for Zeroes}

\description{
  Calculate the geometric mean of a numeric 
  vector, possibly excluding zeroes and/or adding an offset 
  to compensate for zero values.
}

\usage{calcGM(x, offset = 0, exzero = TRUE)}

\arguments{
  \item{x}{vector of numbers}
  \item{offset}{value to add to all components, including zeroes}
  \item{exzero}{if \code{TRUE}, exclude zeroes (but still add the offset)}
}

\value{geometric mean of the modified vector \code{x + offset}}

\note{\code{NA} values are automatically removed from \code{x}}

\examples{
calcGM(c(0,1,100))
calcGM(c(0,1,100),offset=0.01,exzero=FALSE)
}

\keyword{arith}
