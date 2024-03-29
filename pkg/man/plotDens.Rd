\name{plotDens}
\alias{plotDens}
\title{Plot Density Curves from a data frame, matrix, or vector}
\description{
	Plot the density curves from a data frame, matrix, or vector. 
	The mean density curve of the data combined is also shown.
}
\usage{
plotDens(file, clrs=c("blue","red","green","magenta","navy"), ...)  
}
\arguments{
	\item{file}{data frame, matrix, or vector of numeric values.}
	\item{clrs}{vector of colours. Patterns are repeated if the number 
		of fields exceeed the length of \code{clrs}.}
	\item{...}{additional arguments for \code{plot} or \code{lines}.}
}
\details{
	This function is designed primarily to give greater flexibility when viewing 
	results from the R-package \code{BRugs}. Use \code{plotDens} in conjuction with 
	\code{samplesHistory("*",beg=0,plot=FALSE)} rather than \code{samplesDensity} 
	which calls \code{plotDensity}.
}
\examples{
z <- data.frame(y1=rnorm(50,sd=2),y2=rnorm(50,sd=1),y3=rnorm(50,sd=.5))
plotDens(z,lwd=3)
}
\keyword{graphs}