\name{casa-class}
\docType{class}
\alias{casa}
\alias{cva,casa}

\title{Class "casa" catch at age sampling analysis}
\description{Class to receive the output of Na methods.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("casa", ...)}.
This objects contain results from Na methods and are supposed to be used in
further analysis of this results, like CV computation.
}
\section{Slots}{
  \describe{
    \item{\code{desc}:}{Object of class \code{"character"} description}
    \item{\code{nstrata}:}{Object of class \code{"numeric"} number of strata }
    \item{\code{nsample}:}{Object of class \code{"numeric"} number of samples }
    \item{\code{Nl}:}{Object of class \code{"data.frame"} number at length
	estimates }
    \item{\code{varNl}:}{Object of class \code{"data.frame"} number at length
	estimates variance}
    \item{\code{Pal}:}{Object of class \code{"data.frame"} proportion at length
	and age estimates.}
    \item{\code{varPal}:}{Object of class \code{"data.frame"} proportion at length
	and age estimates variance.}
    \item{\code{Na}:}{Object of class \code{"data.frame"} numbers at age
	estimates.}
    \item{\code{varNa}:}{Object of class \code{"data.frame"}  numbers at age
	estimates variance.}
    \item{\code{units}:}{Object of class \code{"vector"} units of the data. }
  }
}
\section{Methods}{
  \describe{
    \item{cva}{\code{signature(object = "casa")}: method to compute coeficients
	of variation for catch at age estimates.  }
  }
}
\references{
Jardim, E.; Trujillo, V. and Sampedro, P. 2004. Uncertainties in sampling procedures 
for age composition of hake and sardine in Iberian Atlantic waters. Scientia
Marina, 68 (4): 561-569 pp.

ICES CM 2004/ACFM:12. Report of the Workshop on Sampling and Calculation Methodology 
for Fisheries Data. Nantes 26-31/01/2004, 20p. 

}
\author{Ernesto Jardim}
\note{none}
\seealso{
}
\examples{
##---- Should be DIRECTLY executable !! ----
# one of these days !!
}
\keyword{classes}
