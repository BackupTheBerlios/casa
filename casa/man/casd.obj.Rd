\name{casd.obj}
\alias{casd.obj}
\docType{data}
\title{sample data from southern hake}
\description{
Sample data from length and age sampling in 1999. 
}
\usage{data(casd.obj)}
\format{
  The format is:
Formal class 'casd' [package ".GlobalEnv"] with 6 slots
  ..@ desc : chr "casa example based on Hake"
  ..@ Wk   :`data.frame':	2 obs. of  4 variables:
  .. ..$ time     : num [1:2] 1 2
  .. ..$ space    : Factor w/ 2 levels "9","C": 1 1
  .. ..$ technical: num [1:2] 2 2
  .. ..$ Wk       : num [1:2] 7783 5104
  ..@ wkv  :`data.frame':	10 obs. of  5 variables:
  .. ..$ time     : num [1:10] 1 1 1 1 2 2 2 2 2 2
  .. ..$ space    : Factor w/ 2 levels "9","C": 1 1 1 1 1 1 1 1 1 1
  .. ..$ technical: num [1:10] 2 2 2 2 2 2 2 2 2 2
  .. ..$ sample   : num [1:10] 1 2 3 4 5 6 7 8 9 10
  .. ..$ wkv      : num [1:10] 7.7 15.3 65.1 3.4 25.4 14 2.1 24.8 19.2 39.1
  ..@ nlkv :`data.frame':	139 obs. of  6 variables:
  .. ..$ time     : num [1:139] 1 1 1 1 1 1 1 1 1 1 ...
  .. ..$ space    : Factor w/ 2 levels "9","C": 1 1 1 1 1 1 1 1 1 1 ...
  .. ..$ technical: num [1:139] 2 2 2 2 2 2 2 2 2 2 ...
  .. ..$ sample   : num [1:139] 1 1 1 1 1 1 1 1 1 1 ...
  .. ..$ len      : int [1:139] 27 28 29 30 31 32 33 34 36 37 ...
  .. ..$ nlkv     : num [1:139] 2 1 5 4 1 3 1 4 1 2 ...
  ..@ oal  :`data.frame':	177 obs. of  6 variables:
  .. ..$ time     : logi [1:177] NA NA NA NA NA NA NA NA NA NA NA NA ...
  .. ..$ space    : logi [1:177] NA NA NA NA NA NA NA NA NA NA NA NA ...
  .. ..$ technical: logi [1:177] NA NA NA NA NA NA NA NA NA NA NA NA ...
  .. ..$ age      : int [1:177] 0 0 1 1 1 1 1 1 1 1 ...
  .. ..$ len      : int [1:177] 17 18 17 18 19 20 21 22 23 24 ...
  .. ..$ oal      : num [1:177] 7 2 12 16 19 20 19 18 16 14 ...
  ..@ units: Named chr [1:4] "kg" "kg" "n" "n"
  .. ..- attr(*, "names")= chr [1:4] "Wk" "wkv" "nlkv" "oal"
}
\details{
This are just 10 samples randomly selected from the complete dataset.
}
\source{
IEO, IPIMAR, AZTI
}
\references{
Jardim, E.; Trujillo, V. and Sampedro, P. 2004. Uncertainties in sampling procedures 
for age composition of hake and sardine in Iberian Atlantic waters. Scientia
Marina, 68 (4): 561-569 pp.

}
\examples{
data(casd.obj)
}
\keyword{datasets}
