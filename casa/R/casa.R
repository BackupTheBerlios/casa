#=====================================================================
#
# Class: casa
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

setClass("casa",
	representation(
		desc="character",
		nstrata="numeric",
		nsample="numeric",
		Nl="data.frame",
		varNl="data.frame",
		Pal="data.frame",
		varPal="data.frame",
		Na="data.frame",
		varNa="data.frame",
		units="vector"
	),
	prototype(
		desc="my stock",
		nstrata=NaN,
		nsample=NaN,
		Nl=data.frame(len=NA, NL=NA),
		varNl=data.frame(len=NA, varNl=NA),
		Pal=data.frame(age=NA, len=NA, Pal=NA),
		varPal=data.frame(age=NA, len=NA, varPal=NA),
		Na=data.frame(age=NA, Na=NA),
		varNa=data.frame(age=NA, varNa=NA),
        units=c(NA,NA,NA,NA)
	)
)

setClass("casa.boot",
	representation(
		obj.casa="casa",
		R="numeric",
		Nl.boot="list",
		Pal.boot="list",
		Na.boot="list",
		alkbystrata="logical",
		fcomb="logical"
	),
	prototype(
		obj.casa=new("casa"),
		R=NaN,
		Nl.boot=list(),
		Pal.boot=list(),
		Na.boot=list(),
		alkbystrata=F,
		fcomb=F
	)
)


#! METHODS

# cva (cv analysis)

setGeneric("cva", function(object){
	standardGeneric("cva")
})

setMethod("cva", "casa", function(object){
	
	Na <- object@Na
	varNa <- object@varNa
	age <- as.numeric(as.character(Na$age))
	
	m1<-merge(object@varNl, object@Pal)	
	varlf <- tapply(m1$varNl*m1$Pal^2, m1$age, sum)
	m2<-merge(object@Nl, object@varPal)
	varaf <- tapply(m2$varPal*m2$Nl^2, m2$age, sum)
	CVlen <- data.frame(age=age, CVlen=sqrt(varlf)/Na$Na)
	CVage <- data.frame(age=age, CVage=sqrt(varaf)/Na$Na)
	CVa <- data.frame(age=age, CVa=sqrt(varNa$varNa)/Na$Na)

	new("cacv",
		desc=object@desc,
		nstrata=object@nstrata,
		nsample=object@nsample,
		Na=object@Na,
		varNa=object@varNa,
		CVa=CVa[order(CVa$age),],
		CVlen=CVlen[order(CVlen$age),],
		CVage=CVage[order(CVage$age),],
		units=object@units)
})


setMethod("cva", "casa.boot", function(object){
	obj.cacv <- cva(object@obj.casa)
	R <- object@R
	age <- as.numeric(as.character(object@obj.casa@Na$age))
	Na.boot <- getrep(object)
	Na.b <- rowMeans(Na.boot, na.rm=T)
	varNa.b <- apply(Na.boot,1,var, na.rm=T)
	corNa.boot <- cor(t(Na.boot), use="complete.obs")

	new("cacv.boot",
		obj.cacv=obj.cacv,
		R=R,
		Na.boot=data.frame(age=age, Na.boot=Na.b),
		varNa.boot=data.frame(age=age, varNa.boot=varNa.b),
		corNa.boot=corNa.boot,
		CVa.boot=data.frame(age=age, CVa.boot=sqrt(varNa.b)/Na.b)
		)
})


# getrep

setGeneric("getrep", function(object){
	standardGeneric("getrep")
})

setMethod("getrep", "casa.boot", function(object){
	vec <- rep(NA, nrow(object@obj.casa@Na))
	age <- as.numeric(as.character(object@obj.casa@Na$age))
	Na.boot <- sapply(object@Na.boot, function(x){
		vec1 <- vec
		vec1[age %in% names(x)]<-x
		vec1			
	})
	dimnames(Na.boot)[[1]] <- age
	Na.boot
})
