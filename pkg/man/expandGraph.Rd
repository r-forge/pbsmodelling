\name{expandGraph}
\alias{expandGraph}
\title{Expand the Plot Area by Adjusting Margins}

\description{
	Optimize the plotting region(s) by minimizing margins.
}

\usage{
expandGraph(mar=c(4,3,1.2,0.5), mgp=c(1.6,.5,0),...)
}

\arguments{
	\item{mar}{numerical vector of the form 'c(bottom, left, top, right)'
		specifying the margins of the plot}
	\item{mgp}{numerical vector of the form 'c(axis title, axis labels, axis line)'
		specifying the margins for axis title, axis labels, and axis line}
	\item{...}{additional graphical parameters to be passed to \code{par}}
}

\seealso{
	\code{\link{resetGraph}}
}

\examples{
resetGraph(); expandGraph(mfrow=c(2,1));
tt=seq(from=-10, to=10, by=0.05);

plot(tt,sin(tt), xlab="this is the x label",  ylab="this is the y label", 
	main="main title", sub="sometimes there is a \"sub\" title")
plot(cos(tt),sin(tt*2), xlab="cos(t)", ylab="sin(2 t)", main="main title", 
	sub="sometimes there is a \"sub\" title")
}

\keyword{device}
