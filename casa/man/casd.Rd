\name{casd-class}
\docType{class}
\alias{casd-class}
\alias{dd,casd-method}
\alias{Na.boot,casd-method}
\alias{Na,casd-method}
\alias{summary,casd-method}

\title{Class "casd" catch at age sampling data }
\description{This class contains the basic data collected from the commercial
activity.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("casd", ...)}.
This objects organize the information from length and age sampling schemes in a
consistent fashion. The data.frames in each slot are similar among them, with
time, space, technical strata definitions.
}
\section{Slots}{
  \describe{
    \item{\code{desc}:}{Object of class \code{"character"} to describe the
	stock. }
    \item{\code{Wk}:}{Object of class \code{"data.frame"} with total weight in
	the strata. }
    \item{\code{wkv}:}{Object of class \code{"data.frame"} with sampled weight
	per sample and strata. }
    \item{\code{nlkv}:}{Object of class \code{"data.frame"} number of
	individuals of length l per sample and strata. }
    \item{\code{oal}:}{Object of class \code{"data.frame"} number of individuals
	of age a and length l observed per strata.}
    \item{\code{units}:}{Object of class \code{"vector"} the units of each slot. }
  }
}
\section{Methods}{
  \describe{
    \item{dd}{\code{signature(obj = "casd")}: delta discrepancy method. }
    \item{Na.boot}{\code{signature(obj = "casd")}: Bootstrao method to estimate catch at age. }
    \item{Na}{\code{signature(obj = "casd")}: Analytical method to estimate
	catch at age. }
    \item{summary}{\code{signature(object = "casd")}: Summary information of the
	data. }
  }
}
\references{
Jardim, E.; Trujillo, V. and Sampedro, P. 2004. Uncertainties in sampling procedures 
for age composition of hake and sardine in Iberian Atlantic waters. Scientia
Marina, 68 (4): 561-569 pp.

ICES CM 2004/ACFM:12. Report of the Workshop on Sampling and Calculation Methodology 
for Fisheries Data. Nantes 26-31/01/2004, 20p. 

Vigneau, J. and Maheevas, S. 2004. Precision in catch at age data with regard to sampling
 design. Working document for the WKSMFD(Nantes 26-31/01/2004).
}
\author{Ernesto Jardim}
\note{Who knows!}

\seealso{
}
\examples{
##---- Should be DIRECTLY executable !! ----
# one of these days there will be an example ...
}
\keyword{classes}
