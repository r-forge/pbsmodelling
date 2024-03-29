\name{promptSaveFile}
\alias{promptSaveFile}
\title{Display a "Save File" Dialogue}

\description{
	Display the default \bold{Save As} prompt provided by the Operating System.
}

\usage{
promptSaveFile(initialfile="", filetype=list(c("*", "All Files")), 
               save=TRUE)
}

\arguments{
	\item{initialfile}{file name of the text file containing the list.}
	\item{filetype}{a list of character vectors indicating file types made available 
		to users of the GUI. Each vector is of length one or two. The first element 
		specifies either the file extension or \code{"*"} for all file types. The second 
		element gives an optional descriptor name for the file type. The supplied 
		\code{filetype} list appears as a set of choices in the pull-down box labelled 
		\dQuote{Files of type:}.}
	\item{save}{logical; if \code{TRUE} display \bold{Save As} prompt, 
		if \code{FALSE} display \bold{Open} prompt.}
}

\value{
	The file name and path of the file selected by the user.
}

\seealso{
	\code{\link{promptOpenFile}}
}

\examples{
\dontrun{
#illustrates how to set filetype.
promptSaveFile("intial_file.txt", filetype=list(c(".txt", "text files"), 
               c(".r", "R files"), c("*", "All Files")))
}
}

\keyword{misc}
