#=====================================================================
#
# Class: cldd
# Date: 25/01/2005
# Version: 0.5.0
# Authors: Ernesto Jardim
#
# Short description: Class for catch at length delta discrepancy
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

# Validity function

clddVal <- function(object){

	if(sum(dim(object@ddv)==dim(object@ddkv))!=2){
		cat("The results of ddv and ddkv have different dimensions !\n") 
	}
	if(sum(dim(object@ddlv)==dim(object@ddlkv))!=2){
		cat("The results of ddlv and ddlkv have different dimensions !\n") 
	}
}

#! CLASS

setClass("cldd",
	representation(
		desc="character",
		nbarlk="data.frame",
		ddlkv="data.frame",
		ddkv="data.frame",
		nbarl="data.frame",
		ddlv="data.frame",
		ddv="data.frame",
		units="vector"
	),
	prototype(
		desc="my stock",
		nbarlk=data.frame(strata=NA, len=NA, nbarlk=NA),
		ddlkv=data.frame(strata=NA, sample=NA, len=NA, ddlkv=NA),
		ddkv=data.frame(strata=NA, sample=NA, ddkv=NA),
		nbarl=data.frame(len=NA, nbarl=NA),
		ddlv=data.frame(strata=NA, sample=NA, len=NA, ddlv=NA),
		ddv=data.frame(strata=NA, sample=NA, ddv=NA),
        units=c(NA,NA,NA,NA)
	),
	validity=clddVal
)

rm(clddVal)

#! METHODS

setMethod("plot", signature(x="cldd", y="missing"), function(x, y,...){

	op <- par(no.readonly = TRUE)
	par(mfcol=c(2,2), mar=c(0,4.5,4.5,0))
	ddlkv <- x@ddlkv
	plot(ddlkv$ddlkv~ddlkv$len, pch=as.character(ddlkv$sample), col=as.numeric(ddlkv$strata), axes=F, main=x@desc, ylab=expression(Delta[lkv]))
	axis(2)
	box()

	par(mar=c(4.5,4.5,0,0))	
	ddlv <- x@ddlv
	plot(ddlv$ddlv~ddlv$len, pch=as.character(ddlv$sample), col=as.numeric(ddlv$strata), axes=F, ylab="", xlab="length")
	axis(2)
	axis(1)
	mtext(expression(Delta[lv]),2,line=3)
	box()

	par(mar=c(0,0,4.5,4.5))
	ddkv <- x@ddkv
	plot(ddkv$ddkv~as.numeric(ddkv$sample), pch=as.character(ddkv$sample), col=as.numeric(ddkv$strata), axes=F, ylab=expression(Delta[kv]))
	axis(4)
	mtext(expression(Delta[kv]),4,line=3)
	box()
	
	par(mar=c(4.5,0,0,4.5))	
	ddv <- x@ddv
	plot(ddv$ddv~as.numeric(ddkv$sample), pch=as.character(ddkv$sample), col=as.numeric(ddv$strata), axes=F, ylab="", xlab="sample")
	axis(4)
	axis(1)
	mtext(expression(Delta[v]),4,line=3)
	box()
	par(op)

})
