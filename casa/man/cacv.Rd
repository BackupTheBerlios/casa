\name{cacv-class}
\docType{class}
\alias{cacv}

\title{Class "cacv" catch at age coeficients of variation}
\description{Class to receive the results of cv methods.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("cacv", ...)}.
This class contains the results of cv methods, like cva, that compute
coeficients of variation for catch at age.}
\section{Slots}{
  \describe{
    \item{\code{desc}:}{Object of class \code{"character"} description }
    \item{\code{nstrata}:}{Object of class \code{"numeric"} number of strata}
    \item{\code{nsample}:}{Object of class \code{"numeric"} number of samples }
    \item{\code{Na}:}{Object of class \code{"data.frame"} catch at age }
    \item{\code{varNa}:}{Object of class \code{"data.frame"} catch at age
	variance }
    \item{\code{CVa}:}{Object of class \code{"data.frame"} catch at age CV }
    \item{\code{CVlen}:}{Object of class \code{"data.frame"} catch at age CV (length
	component).}
    \item{\code{CVage}:}{Object of class \code{"data.frame"} catch at age CV
	(age component) }
    \item{\code{units}:}{Object of class \code{"vector"} units }
  }
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "cacv", y = "missing")}: plots CV at age in
	a nice way that can be printed or exported. }
  }
}
\references{
Jardim, E.; Trujillo, V. and Sampedro, P. 2004. Uncertainties in sampling procedures 
for age composition of hake and sardine in Iberian Atlantic waters. Scientia
Marina, 68 (4): 561-569 pp.
}
\author{Ernesto Jardim}
\note{none}

\seealso{
}
\examples{
##---- Should be DIRECTLY executable !! ----
# Not for the moment ...
}
\keyword{classes}
