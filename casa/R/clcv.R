#=====================================================================
#
# Class: clcv
# Date: 25/01/2005
# Version: 0.5.0
# Authors: Ernesto Jardim
#
# Short description: Class for catch at age sampling analysis
#
# Slots: 
#
# Methods: 
#
# ToDo: 
#
# References (bibtex):
#	WD WKSCMFD05
#
#!Notes: this is where the results from Na methods are stored
#
#=====================================================================

#! CLASS

setClass("clcv",
	representation(
		desc="character",
		nstrata="numeric",
		nsample="numeric",
		Nl="data.frame",
		varNl="data.frame",
		CVl="data.frame",
		units="vector"
	),
	prototype(
		desc="my stock",
		nstrata=NaN,
		nsample=NaN,
		Nl=data.frame(len=NA, Nl=NA),
		varNl=data.frame(len=NA, varNl=NA),
		CVl=data.frame(len=NA, CVl=NA),
        units=c(NA,NA,NA,NA)
	)
)

#! METHODS

# plot

setMethod("plot", signature(x="clcv", y="missing"), function(x, y,...){

	op <- par(no.readonly = TRUE)
	stop("under development")
	
	par(op)
})
