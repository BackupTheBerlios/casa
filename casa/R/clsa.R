#=====================================================================
#
# Class: clsa
# Date: 25/01/2005
# Version: 0.5.0
# Authors: Ernesto Jardim
#
# Short description: Class for catch at length sampling analysis
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

setClass("clsa",
	representation(
		desc="character",
		nstrata="numeric",
		nsample="numeric",
		Nl="data.frame",
		varNl="data.frame",
		units="vector"
	),
	prototype(
		desc="my stock",
		nstrata=NaN,
		nsample=NaN,
		Nl=data.frame(len=NA, NL=NA),
		varNl=data.frame(len=NA, varNl=NA),
        units=c(NA,NA,NA,NA)
	)
)

#! METHODS

# cva (cv analysis)

setMethod("cva", "clsa", function(object){
	
	Nl <- object@Nl
	varNl <- object@varNl
	CVl <- data.frame(len=Nl$len, CVl=sqrt(varNl$varNl)/Nl$Nl)

	new("clcv",
		desc=object@desc,
		nstrata=object@nstrata,
		nsample=object@nsample,
		CVl=CVl[order(CVl$len),],
		units=object@units)
})

