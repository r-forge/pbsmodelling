\name{CCA.qbr}
\docType{data}
\alias{CCA.qbr}
\title{Dataset: Sampled Counts of Quillback Rockfish 
(Sebastes maliger)}

\description{
  Count of sampled fish-at-age for quillback rockfish 
  (\emph{Sebastes maliger}) in Johnstone Strait, British Columbia, 
  from 1984 to 2004.
}

\usage{data(CCA.qbr)}

\format{
  A matrix with 70 rows (ages) and 14 columns 
  (years). Attributes \dQuote{syrs} and \dQuote{cyrs} 
  specify years of survey and commercial data, respectively.

  \tabular{ll}{
    \code{[,c(3:5,9,13,14)]} \tab Counts-at-age from research
      survey samples \cr
    \code{[,c(1,2,6:8,10:12)]} \tab Counts-at-age from commercial
      fishery samples \cr
  }

  All elements represent sampled counts-at-age in year. Zero-value
  entries indicate no observations.
}

\note{Years 1994, 1997-1999, and 2002-2003 do not have data.}

\details{
  Handline surveys for rockfish have been conducted in 
  Johnstone Strait (British Columbia) and adjacent waterways 
  (126\eqn{^\circ}{�}37'W to 126\eqn{^\circ}{�}53'W, 
  50\eqn{^\circ}{�}32'N to 50\eqn{^\circ}{�}39'N) since 1986. 
  Yamanaka and Richards (1993) describe surveys conducted in 1986, 
  1987, 1988, and 1992. In 2001, the Rockfish Selective Fishery 
  Study (Berry 2001) targeted quillback rockfish \emph{Sebastes 
  maliger} for experiments on improving survival after capture  by 
  hook and line gear. The resulting data subsequently have been 
  incorporated into the survey data series. The most recent survey 
  in 2004 essentially repeated the 1992 survey design. Fish samples 
  from surveys have been supplemented by commercial handline 
  fishery samples taken from a larger region 
  (126\eqn{^\circ}{�}35'W to 127\eqn{^\circ}{�}39'W, 
  50\eqn{^\circ}{�}32'N to 50\eqn{^\circ}{�}59'N) in the years 
  1984-1985, 1989-1991, 1993, 1996, and 2000 (Schnute and Haigh 
  2007).
}

\source{
  Fisheries and Oceans Canada - GFBio database: \cr
  \url{http://www-sci.pac.dfo-mpo.gc.ca/sa-mfpd/statsamp/StatSamp_GFBio.htm}
}

\references{
  Berry, M.D. 2001. Area 12 (Inside) Rockfish Selective Fishery 
  Study. Science Council of British Columbia, Project Number FS00-
  05.
  
  Schnute, J.T., and Haigh, R. 2007. Compositional analysis of 
  catch curve data with an application to \emph{Sebastes maliger}. 
  ICES Journal of Marine Science (in press).
  
  Yamanaka, K.L. and Richards, L.J. 1993. 1992 Research catch and 
  effort data on nearshore reef-fishes in British Columbia 
  Statistical Area 12. Canadian Manuscript Report of Fisheries and 
  Aquatic Sciences 2184, 77 pp.
}

\examples{
# Plot age proportions (blue bubbles = survey data, red = commercial)
data("CCA.qbr", package="PBSmodelling")
z <- CCA.qbr; cyr <- attributes(z)$cyrs;
z <- apply(z,2,function(x){x/sum(x)}); z[,cyr] <- -z[,cyr];
x <- as.numeric(dimnames(z)[[2]]); xlim <- range(x) + c(-.5,.5);
y <- as.numeric(dimnames(z)[[1]]); ylim <- range(y) + c(-1,1);
plotBubbles(z,xval=x,yval=y,powr=.5,size=0.15,lwd=1,clrs=c("blue","red"),
            xlim=xlim,ylim=ylim,xlab="Year",ylab="Age",cex.lab=1.5)
}

\keyword{datasets}
