\name{getWinVal}
\alias{getWinVal}
\title{Retreive Widget Values for Use in R Code}

\description{
	Get a list of variables defined and set by the GUI widgets. An optional 
	argument \code{scope} directs the function to create local or global 
	variables based on the list that is returned.
}

\usage{
getWinVal(v=NULL, scope="", asvector=FALSE, winName="")
}

\arguments{
	\item{v}{vector of variable names to retrieve from the GUI widgets. 
		If \code{NULL}, \code{v} retrieves all variables from all GUI widgets.}
	\item{scope}{scope of the retrieval. The default sets no variables in the non-GUI 
		environment; \code{scope="L"} creates variables locally in relation to the 
		parent frame that called the function; and \code{scope="G"} creates global variables(\code{pos=1}).}
	\item{asvector}{return a vector instead of a list. 
		WARNING: if a widget variable defines a true vector or matrix, this will not work.}
	\item{winName}{window from which to select GUI widget values. The default 
		takes the window that has most recently received new user input.}
}

\value{
	A list (or vector) with named components, where names and values are defined by GUI widgets.
}

\seealso{
	\code{\link{parseWinFile}}, \code{\link{setWinVal}}, \code{\link{clearWinVal}}
}

\keyword{methods}
