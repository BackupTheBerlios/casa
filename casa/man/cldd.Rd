\name{cldd-class}
\docType{class}
\alias{cldd-class}
\alias{plot,cldd,missing-method}

\title{Class "cldd" catch at length delta discrepancy}
\description{Class to receive the results of delta discrepancy method.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("cldd", ...)}.
This objects contain the results of delta discrepancy methods and the mean
number of individuals per unit weight.}
\section{Slots}{
  \describe{
    \item{\code{desc}:}{Object of class \code{"character"} description }
    \item{\code{nbarlk}:}{Object of class \code{"data.frame"} mean number of
	individuals per unit weight and strata. }
    \item{\code{ddlkv}:}{Object of class \code{"data.frame"} delta discrepancy
	per length, strata and sample.}
    \item{\code{ddkv}:}{Object of class \code{"data.frame"}  delta discrepancy
	per strata and sample.}
    \item{\code{nbarl}:}{Object of class \code{"data.frame"}  mean number of
	individuals per unit weight.}
    \item{\code{ddlv}:}{Object of class \code{"data.frame"}  delta discrepancy
	per length and sample.}
    \item{\code{ddv}:}{Object of class \code{"data.frame"}  delta discrepancy
	per sample.}
    \item{\code{units}:}{Object of class \code{"vector"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "cldd", y = "missing")}: plots delta
	discrepancy results in a nice way that can be printed or exported. }
  }
}
\references{
Vigneau, J. and Maheevas, S. 2004. Precision in catch at age data with regard to sampling
 design. Working document for the WKSMFD(Nantes 26-31/01/2004).

}
\author{Ernesto Jardim}
\note{none}

\seealso{
}
\examples{
##---- Should be DIRECTLY executable !! ----
# still working on one ...
}
\keyword{classes}
