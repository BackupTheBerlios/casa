#=====================================================================
#
# Class: casd
# Date: 25/01/2005
# Version: 0.6.1
# Authors: Ernesto Jardim
#
# Short description: Class for catch at age sampling data
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
#!Notes: 
#
#=====================================================================

#! CLASS
# validity function

casdVal <- function(object){

	Wk <- object@Wk
	wkv <- object@wkv
	nlkv <- object@nlkv
	oal <- object@oal[object@oal$oal!=0,]
	units <- object@units

	# check names
	if(sum(names(Wk) %in% c("time", "space", "technical", "Wk"))!=4){
		stop("\nCASA: Wk with wrong names ! Rename variables to \"time\", \"space\", \"technical\", \"Wk\"", "\n", call=F)
	}
	if(sum(names(wkv) %in% c("time", "space", "technical", "sample", "wkv"))!=5){
		stop("\nCASA: wkv with wrong names ! Rename variables to \"time\", \"space\", \"technical\", \"sample\", \"wkv\"", "\n")
	}
	if(sum(names(nlkv) %in% c("time", "space", "technical", "sample", "len", "nlkv"))!=6){
		stop("\nCASA: nlkv with wrong names ! Rename variables to \"time\", \"space\", \"technical\", \"sample\", \"len\", \"nlkv\"", "\n")
	}
	if(sum(names(oal) %in% c("time", "space", "technical", "len", "age", "oal"))!=6){
		stop("\nCASA: oal with wrong names ! Rename variables to \"time\", \"space\", \"technical\", \"len\", \"age\", \"oal\"", "\n")
	}

	Wk.strata <- paste(Wk$time, Wk$space, Wk$technical, sep="")
	wkv.strata <- paste(wkv$time, wkv$space, wkv$technical, sep="")
	nlkv.strata <- paste(nlkv$time, nlkv$space, nlkv$technical, sep="")
	oal.strata <- paste(oal$time, oal$space, oal$technical, sep="")
	
	retval <- NULL
	# check weight units
	if(!is.na(units[1]))
	{
		if(units[1]!=units[2])
		{
			retval <- c(retval, "\nCASA: weights are in different units")
		}
		# check catch numbers units
		if(units[3]!=units[4])
		{
			retval <- c(retval, "\nCASA: catch numbers are in different units\n")
		}
	}
	# samples weight and lengths match ?
	v1 <- sort(paste(wkv.strata, wkv$sample))
	v2 <- sort(unique(paste(nlkv.strata, nlkv$sample))) 
	if(!identical(v1, v2))
	{
		retval <- c(retval, "\nCASA: sample weights and sample lengths mismatch in strata and/or sample")
	}
	# total weight and samples weight match ?
	v1 <- sort(unique(wkv.strata))
	v2 <- sort(Wk.strata) 
	if(length(v1 %in% v2)!=length(unique(wkv.strata)))
	{
		retval <- c(retval, "\nCASA: total weights and sample weights mismatch in strata")
	}
	# ALK covers length distribution
	if(length(unique(nlkv$len)) > length(unique(oal$len)))
	{
		retval <- c(retval, "\nCASA: some lengths don't exist in the ALK")
	}

	if(is.null(retval)) return(TRUE)
	else return(retval)

}


# class

setClass("casd",
	representation(
		desc="character",
		Wk="data.frame",
		wkv="data.frame",
		nlkv="data.frame",
		oal="data.frame",
		units="vector"
	),
	prototype(
		desc="my stock",
		Wk=data.frame(time=NA, space=NA, technical=NA, Wk=NA),
		wkv=data.frame(time=NA, space=NA, technical=NA, sample=NA, wkv=NA),
		nlkv=data.frame(time=NA, space=NA, technical=NA, sample=NA, len=NA, nlkv=NA),
		oal=data.frame(time=NA, space=NA, technical=NA, age=NA, len=NA, oal=NA),
        units=c(NA,NA,NA,NA)
	),
	validity=casdVal
)

rm(casdVal)

#! METHODS

# Na

setGeneric("Na", function(obj, alkbystrata=F, ...){
	standardGeneric("Na")
})

setMethod("Na", "casd", function(obj, alkbystrata=F){

    Wk <- obj@Wk
    wkv <- obj@wkv
    nlkv <- obj@nlkv
    oal <- obj@oal

	# concatenate into a single strata
    Wk <- data.frame(strata = paste(Wk$time, Wk$space, Wk$technical), 
        Wk = Wk$Wk)
    wkv <- data.frame(strata = paste(wkv$time, wkv$space, wkv$technical), 
        sample = wkv$sample, wkv = wkv$wkv)
    nlkv <- data.frame(strata = paste(nlkv$time, nlkv$space, 
        nlkv$technical), sample = nlkv$sample, len = nlkv$len, 
        nlkv = nlkv$nlkv)
    oal <- data.frame(strata = paste(oal$time, oal$space, oal$technical), 
        age = oal$age, len = oal$len, oal = oal$oal)

	# Nl
	Nlk <- Nl(obj)
	# Pal
	Palk <- Pal(obj)

	#> Na
	if(alkbystrata==T){
		
		Na <- merge(Nlk, Palk)
		Na <- lapply(split(Na,Na$age),function(x){
			Na <- sum(x$Palk*x$Nl)
			varNa <- sum(x$Palk^2*x$varNl) + sum(x$varPalk*x$Nl^2)+sum(x$varPalk*x$varNl)
			data.frame(Na=Na, varNa=varNa)
		})
		Na <- do.call("rbind", Na)

	} else {
	
		Nl <- tapply(Nlk$Nlk, Nlk$len, sum)
		varNl <- tapply(Nlk$varNlk, Nlk$len, sum)
		Na <- merge(data.frame(len=names(Nl),Nl=Nl, varNl=varNl),Palk[,c(2,3,5,6)])	
		Na <- lapply(split(Na,Na$age),function(x){
			Na <- sum(x$Palk*x$Nl)
			varNa <- sum(x$Palk^2*x$varNl) + sum(x$varPalk*x$Nl^2)+sum(x$varPalk*x$varNl)
			data.frame(Na=Na, varNa=varNa)
		})
		Na <- do.call("rbind", Na)
		Pal <- Palk
		dimnames(Pal)[[1]] <- 1:nrow(Pal)
	}


	new("casa", 
		desc=obj@desc, 
		nstrata=length(Wk$strata),
		nsample=length(wkv$sample),
		Nl=data.frame(len=names(Nl), Nl=Nl),
		varNl=data.frame(len=names(varNl), varNl=varNl),
		Pal=data.frame(Pal[,c(2,3,5)]),
		varPal=data.frame(Pal[,c(2,3,6)]),
		Na=data.frame(age=dimnames(Na)[[1]], Na=Na$Na), 
		varNa=data.frame(age=dimnames(Na)[[1]], varNa=Na$varNa),
		units=obj@units)

})

# Nl
#? Incluir Wk e & Co. nesta função ?
setGeneric("Nl", function(obj, ...){
	standardGeneric("Nl")
})

setMethod("Nl", "casd", function(obj){

	Wk <- obj@Wk
	wkv <- obj@wkv
	nlkv <- obj@nlkv

	# concatenate into single level strata
	Wk <- data.frame(strata=paste(Wk$time, Wk$space, Wk$technical), Wk=Wk$Wk)
	wkv <- data.frame(strata=paste(wkv$time, wkv$space, wkv$technical), sample=wkv$sample, wkv=wkv$wkv)
	nlkv <- data.frame(strata=paste(nlkv$time, nlkv$space, nlkv$technical), sample=nlkv$sample, len=nlkv$len, nlkv=nlkv$nlkv)

	#> further checks
	# check no-sampled strata
	v1 <- sort(unique(wkv$strata))
	v2 <- sort(Wk$strata) 
	if(length(v2 %in% v1)!=length(v2))
	{
		stop(" Some strata were not sampled! \n Please allocate those landings to sampled strata or remove them and deal with it later with SOP! \n")
	}

	#> Nl
	ldata <- merge(merge(nlkv, wkv),Wk)
	Nlk <- lapply(split(ldata, ldata$strata), function(x){
		Wk <- unique(x$Wk)
		wk <- sum(unique(x$wkv))
		nlk <- tapply(x$nlkv, x$len,sum)
		Nlk <- nlk*Wk/wk
		Vk <- length(unique(x$sample))
		mat <- merge(x, data.frame(len=names(nlk), nbar=nlk/wk))
		t1 <- 1/(Vk*(Vk-1))
		t2 <- tapply((mat$nlkv/mat$wkv-mat$nbar)^2, x$len, sum)
		varNlk <- Wk^2*t1*t2
		data.frame(strata=x$strata[1], len=names(nlk), Nlk=Nlk, varNlk=varNlk)
	}) 

do.call("rbind", Nlk)

})

# Pal
setGeneric("Pal", function(obj, ...){
	standardGeneric("Pal")
})

setMethod("Pal", "casd", function(obj){

    oal <- obj@oal
    oal <- data.frame(strata = paste(oal$time, oal$space, oal$technical), 
        age = oal$age, len = oal$len, oal = oal$oal)

	#> Pal
	Palk <- lapply(split(oal,list(oal$strata, oal$len)), function(x) {
		ol <- sum(x$oal)
		x$Palk <- x$oal/ol
		x$varPalk <- x$Palk*(1-x$Palk)/(ol-1)
		x		
	})
do.call("rbind", Palk)

})

# dd
setGeneric("dd", function(obj, ...){
	standardGeneric("dd")
})

setMethod("dd", "casd", function(obj){

	Wk <- obj@Wk
	wkv <- obj@wkv
	nlkv <- obj@nlkv
	units <- obj@units

	# concatenate into single level strata
	Wk <- data.frame(strata=paste(Wk$time, Wk$space, Wk$technical), Wk=Wk$Wk)
	wkv <- data.frame(strata=paste(wkv$time, wkv$space, wkv$technical), sample=wkv$sample, wkv=wkv$wkv)
	nlkv <- data.frame(strata=paste(nlkv$time, nlkv$space, nlkv$technical), sample=nlkv$sample, len=nlkv$len, nlkv=nlkv$nlkv)

	#> Nl
	ldata <- merge(nlkv, wkv)
	ddk <- lapply(split(ldata, ldata$strata), function(x){
		wk <- sum(unique(x$wkv))
		nlk <- tapply(x$nlkv, x$len,sum)
		nbarlk <- data.frame(len=names(nlk), nbarlk=nlk/wk)
		ddlkv <- merge(x, nbarlk)
		ddlkv$ddlkv <- ddlkv$nlkv-ddlkv$nbarlk*ddlkv$wkv  
		ddkv <- tapply(ddlkv$ddlkv, ddlkv$sample, sum)
		ddkv <- data.frame(strata=x$strata[1], sample=rownames(ddkv), ddkv=ddkv)

		list(ddlkv=ddlkv, ddkv=ddkv)	
	}) 

	ddlkv <- lapply(ddk, function(x) x$ddlkv)
	ddlkv <- do.call("rbind", ddlkv)
	ddkv <- lapply(ddk, function(x) x$ddkv)
	ddkv <- do.call("rbind", ddkv)
	nbarlk <- unique(ddlkv[,c(2,1,6)])
	
	w <- sum(unique(ldata$wkv))
	nl <- tapply(ldata$nlkv, ldata$len, sum)
	nbarl <- data.frame(len=names(nl), nbarl=nl/w)
	ddlv <- merge(ldata, nbarl)
	ddlv$ddlv <- ddlv$nlkv-ddlv$nbarl*ddlv$wkv  
	ddv <- tapply(ddlv$ddlv, ddlv$sample, sum)
	ddv <- merge(unique(ddlv[,c(2,3)]), data.frame(sample=rownames(ddv), ddv=ddv))

	new("cldd",
	desc=obj@desc,
	nbarlk=nbarlk,
	ddlkv=ddlkv[,c(2,3,1,7)],
	ddkv=ddkv,
	nbarl=nbarl,
	ddlv=ddlv[,c(2,3,1,7)], 
	ddv=ddv[,c(2,1,3)],
	units=obj@units)

})

# Na.boot
setGeneric("Na.boot", function(obj, R, type, fcomb=F, alkbystrata=F...){
	standardGeneric("Na.boot")
})

setMethod("Na.boot", "casd", function(obj, R, type=c("Nl","Pal","Na"), fcomb=F, alkbystrata=F){

#	stop("SORRY, THIS IS STILL UNDER DEVELOPMENT !\n")	

	obj.casa <- Na(obj, alkbystrata)
	agevec <- as.numeric(as.character(obj.casa@Na$age))
	lenvec <- as.numeric(as.character(obj.casa@Nl$len))

	Wk <- obj@Wk
	wkv <- obj@wkv
	
	# concatenate into single level strata
	Wk <- data.frame(strata=paste(Wk$time, Wk$space, Wk$technical), Wk=Wk$Wk)
	wkv <- data.frame(strata=paste(wkv$time, wkv$space, wkv$technical), sample=wkv$sample, wkv=wkv$wkv)

	# Nl
	
	if(type=="Nl" | type=="Na"){
		nlkv <- obj@nlkv
		nlkv <- data.frame(strata=paste(nlkv$time, nlkv$space, nlkv$technical), sample=nlkv$sample, len=nlkv$len, nlkv=nlkv$nlkv)
		slen <- split(1:R,1:R)
		for(i in 1:R){
			bsvec <- lapply(split(nlkv, nlkv$strata), function(x){
				svec <- unique(x$sample)
				sample(svec, length(svec), replace=T)
			})
		
			bsvec <- unlist(bsvec)
			bnlkv <- obj@nlkv[obj@nlkv$sample %in% bsvec,]
			bwkv <- obj@wkv[obj@wkv$sample %in% bsvec,]
			sobj <- obj
			sobj@nlkv <- bnlkv
			sobj@wkv <- bwkv
			slen[[i]] <- Nl(sobj) 
		}	
	}
		
	# Pal
	if(type=="Pal" | type=="Na"){
		oal <- obj@oal
		oal <- data.frame(strata=paste(oal$time, oal$space, oal$technical), age=oal$age, len=oal$len, oal=oal$oal)

		soal <- split(1:R,1:R)
		for(i in 1:R){
			bsk <- lapply(split(obj@oal, oal$strata), function(x){
				bslk <- lapply(split(x,x$len), function(y){
					if(nrow(y)==1){
						y
					} else {
					bs <- sample(y$age, sum(y$oal), replace=T, prob=y$oal)
					bs <- tapply(bs, bs, length)
					data.frame(time=y$time[1], space=y$space[1], technical=y$technical[1], age=as.numeric(names(bs)), len=y$len[1], oal=bs)
					}
				})
				do.call("rbind", bslk) 
			})
			boal <- do.call("rbind", bsk)
			sobj <- obj
			sobj@oal <- boal
			soal[[i]] <- Pal(sobj) 
		}	
	}

	# Na.boot
	Na.boot.lst <- split(1:R,1:R)
	if(fcomb==T){

		
	} else {
		if(alkbystrata==T){
			for(i in 1:R){
				mat <- merge(slen[[i]], soal[[i]])
				Na.boot<-tapply(mat$Nlk*mat$Palk, mat$age, sum)
			}	
		} else {
			for(i in 1:R){
				mat <- merge(slen[[i]], soal[[i]][,-1])
				Na.boot.lst[[i]]<-tapply(mat$Nlk*mat$Palk, mat$age, sum)
			}	
		}
	}

	
	new("casa.boot",
		obj.casa=obj.casa,
		R=R,
		Nl.boot=slen,
		Pal.boot=soal,
		Na.boot=Na.boot.lst,
		alkbystrata=alkbystrata,
		fcomb=fcomb
	)		
})


# summary

setMethod("summary", "casd", function(object){

	cat("object of class \"casd\" \n")
	cat("description          :", object@desc, "\n")
	cat("total landings       :", sum(object@Wk$Wk), object@units[1], "\n")
	cat("total sampled weigth :", sum(object@wkv$wkv), object@units[2],"\n")
	nstrata <- length(unique(paste(object@Wk$time, object@Wk$space, object@Wk$technical)))
	cat("number of strata     :", nstrata, "\n")
	cat("lengths              : [", min(object@nlkv$len), ",",max(object@nlkv$len),  "]\n", sep="")
	cat("ages                 :", sort(unique(object@oal$age)), "\n")

})

#! FUNCTIONS

casd  <-  function(desc, Wk, wkv, nlkv, oal, units) {
	
	obj <- new("casd")

	# transform so that factors have the same levels and no blanks
	Wk <- tof(Wk)		
	wkv <- tof(wkv)		
	nlkv <- tof(nlkv)		
	oal <- tof(oal)
		
	if (!missing(desc)) obj@desc <- desc
	if (!missing(Wk)) obj@Wk <- Wk
	if (!missing(wkv)) obj@wkv <- wkv
	if (!missing(nlkv)) obj@nlkv <- nlkv
	if (!missing(oal)) obj@oal <- oal[oal$oal!=0,]
	if (!missing(units)) obj@units <- units
#	names(obj@units)=c("Wk","wkv","nlkv","oal")
	
	if(validObject(obj)) return(obj) else validObject(obj)
	
}

setGeneric("tof", function(object, ...){
	standardGeneric("tof")
})

setMethod("tof", signature("factor"), function(object){
	object <- as.character(object)
	object <- gsub("[ ]+", "", object)
	object <- factor(object)
	object
})

setMethod("tof", signature("character"), function(object){
	object <- gsub("[ ]+", "", object)
	object <- factor(object)
	object
})

setMethod("tof", signature("data.frame"), function(object){
	for(i in 1:ncol(object)){
		if(is.factor(object[,i]) | is.character(object[,i])){
			object[,i] <- tof(object[,i])
		} else {
			object[,i] <- object[,i]
		}
	}
	object	
})
