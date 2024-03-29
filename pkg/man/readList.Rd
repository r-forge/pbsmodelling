\name{readList}
\alias{readList}
\title{Read a List from a File in PBS Modelling Format}

\description{
	Read in a list previously saved to a file by \code{writeList}. 
	At present, only two formats are supported - R's native format 
	used by the \code{dput} function or an ad hoc \code{PBSmodelling} format. 
	The function \code{readList} detects the format automatically.

	For information about the \code{PBSmodelling} format, see \code{\link{writeList}}.
}

\usage{readList(fname)}

\arguments{
	\item{fname}{file name of the text file containing the list.}
}

\seealso{
\code{\link{writeList}}, \code{\link{unpackList}}
}

\keyword{list}
\keyword{file}
