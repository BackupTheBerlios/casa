#=====================================================================
#
# Class: cacv
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

setClass("cacv",
	representation(
		desc="character",
		nstrata="numeric",
		nsample="numeric",
		Na="data.frame",
		varNa="data.frame",
		CVa="data.frame",
		CVlen="data.frame",
		CVage="data.frame",
		units="vector"
	),
	prototype(
		desc="my stock",
		nstrata=NaN,
		nsample=NaN,
		Na=data.frame(age=NA, Na=NA),
		varNa=data.frame(age=NA, varNa=NA),
		CVa=data.frame(age=NA, CVa=NA),
		CVlen=data.frame(age=NA, CVlen=NA),
		CVage=data.frame(age=NA, CVage=NA),
        units=c(NA,NA,NA,NA)
	)
)


setClass("cacv.boot",
	representation(
		obj.cacv="cacv",
		R="numeric",
		Na.boot="data.frame",
		varNa.boot="data.frame",
		corNa.boot="matrix",
		CVa.boot="data.frame"
	),
	prototype(
		obj.cacv=new("cacv"),
		R=NaN,
		Na.boot=data.frame(age=NA, Na.boot=NA),
		varNa.boot=data.frame(age=NA, varNa.boot=NA),
		corNa.boot=matrix(),
		CVa.boot=data.frame(age=NA, CVa.boot=NA)
	)
)

#! METHODS

# plot

setMethod("plot", signature(x="cacv", y="missing"), function(x, y,...){

	op <- par(no.readonly = TRUE)
	par(mfrow=c(2,1), mar=c(0,4.5,4.5,4.5))
	age <- as.numeric(as.character(x@Na$age))	
	ic95 <- apply(cbind(x@Na$Na,x@varNa$varNa),1,function(z){
		z[1]+qt(c(0.025,0.975),x@nsample-1)*sqrt(z[2])
	})
	ymax <- max(ic95, na.rm=T)+0.05*max(ic95, na.rm=T)
	ymin <- min(ic95, na.rm=T)-0.05*min(ic95, na.rm=T)
	plot(x@Na$Na~age, pch=19, ylim=c(ymin, ymax), main=x@desc, axes=F, ylab="Na", xlab="age")
	arrows(age,x@Na$Na,age,ic95[1,], length=0.1, angle=90)
	arrows(age,x@Na$Na,age,ic95[2,], length=0.1, angle=90)
	axis(2)
	box()
	legend(age[1], ymax, legend=c("Na", "ic95"), pch=c(19,NA), lty=c(0,1), bty="n")

	par(mar=c(4.5,4.5,0,4.5))	
	plot(x@CVa$CVa~age, ylim=c(0,1), type="l", axes=F, ylab="")
	lines(x@CVage$CVage~age, lty=2)
	lines(x@CVlen$CVlen~age, lty=3)
	axis(4)
	axis(1)
	box()
	mtext("CV", 4, line=3)
	legend(age[1], 1, legend=c("CV Na", "CV age", "CV len"), lty=c(1,2,3), bty="n")
	
	par(op)
})


setMethod("plot", signature(x="cacv.boot", y="missing"), function(x, y,...){

	op <- par(no.readonly = TRUE)
	par(mfrow=c(2,1), mar=c(0,4.5,4.5,4.5))
	age <- as.numeric(as.character(x@obj.cacv@Na$age))	
	ic95 <- apply(cbind(x@Na.boot$Na.boot,x@varNa.boot$varNa.boot),1,function(z){
		z[1]+qt(c(0.025,0.975),x@R-1)*sqrt(z[2])
	})
	ymax <- max(ic95, na.rm=T)+0.05*max(ic95, na.rm=T)
	ymin <- min(ic95, na.rm=T)-0.05*min(ic95, na.rm=T)
	plot(x@Na.boot$Na.boot~age, pch=19, ylim=c(ymin, ymax), main=x@obj.cacv@desc, axes=F, ylab="Na", xlab="age")
	arrows(age,x@Na.boot$Na.boot,age,ic95[1,], length=0.1, angle=90)
	arrows(age,x@Na.boot$Na.boot,age,ic95[2,], length=0.1, angle=90)
	axis(2)
	box()
	legend(age[1], ymax, legend=c("Na", "ic95"), pch=c(19,NA), lty=c(0,1), bty="n")

	par(mar=c(4.5,4.5,0,4.5))	
	plot(x@CVa.boot$CVa.boot~age, ylim=c(0,1), type="l", axes=F, ylab="")
#	lines(x@CVage$CVage~age, lty=2)
#	lines(x@CVlen$CVlen~age, lty=3)
	axis(4)
	axis(1)
	box()
	mtext("CV by bootstrap", 4, line=3)
#	legend(age[1], 1, legend=c("CV Na", "CV age", "CV len"), lty=c(1,2,3), bty="n")
	
	par(op)
})
