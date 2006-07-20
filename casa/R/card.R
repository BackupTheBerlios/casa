#? usar alloc para lw ?

setClass("card",
	representation(
		desc="character",
		Wk="data.frame",
		Wkvc="data.frame",
		wkvc="data.frame",
		nlkvc="data.frame",
		alk="data.frame",
		lwk="data.frame",
		alloclen="data.frame",
		alloclan="data.frame",
		allocalk="data.frame"
	),
	prototype(
		desc="description",
		Wk=data.frame(time="all", space="all", tech="all", sample="all",  ccat="all", len="all", age="all", wt=NA, freq=NA),
		Wkvc=data.frame(time="all", space="all", tech="all", sample="all",  ccat="all", len="all", age="all", wt=NA, freq=NA),
		wkvc=data.frame(time="all", space="all", tech="all", sample="all",  ccat="all", len="all", age="all", wt=NA, freq=NA),
		nlkvc=data.frame(time="all", space="all", tech="all", sample="all",  ccat="all", len="all", age="all", wt=NA, freq=NA),
		alk=data.frame(time="all", space="all", tech="all", sample="all",  ccat="all", len="all", age="all", wt=NA, freq=NA),
		lwk=data.frame(time="all", space="all", tech="all", sample="all",  ccat="all", len="all", age="all", wt=NA, freq=NA),
		alloclen=data.frame(time=NA, space=NA, tech=NA, alloctime=NA, allocspace=NA, alloctech=NA),
		alloclan=data.frame(time=NA, space=NA, tech=NA, alloctime=NA, allocspace=NA, alloctech=NA),
		allocalk=data.frame(time=NA, space=NA, tech=NA, alloctime=NA, allocspace=NA, alloctech=NA)		
	)
)

setGeneric("card", function(object, ...){
	standardGeneric("card")
	}
)

setMethod("card", signature("list"), function(object, ...){
	nm <- names(object)
	v <- match(nm, c("desc", "Wk", "Wkvc", "wkvc", "nlkvc", "alk", "lwk")) 
	if(length(v)!=7) stop("The list does not have all the necessary data.frames or has no names !\n")
	if(is.na(match("desc", nm))){
		desc <- c("description")
	} else {
		desc <- object$desc
	}
	Wk <- object$Wk
	Wkvc <- object$Wkvc
	wkvc <- object$wkvc
	nlkvc <- object$nlkvc
	alk <- object$alk
	lwk <- object$lwk

	for(i in 1:5){
		Wk[,i] <- f2f(Wk[,i])
		Wkvc[,i] <- f2f(Wkvc[,i])
		wkvc[,i] <- f2f(wkvc[,i])
		nlkvc[,i] <- f2f(nlkvc[,i])
		alk[,i] <- f2f(alk[,i])
		lwk[,i] <- f2f(lwk[,i])
	}
	
	# fullfill the alloc tables
	alloclen <- fat(nlkvc[,1:3])
	alloclan <- fat(Wk[,1:3])
	allocalk <- fat(alk[,1:3])
	
	# new object
	new("card", desc=desc, Wk=Wk, wkvc=wkvc, Wkvc=Wkvc, nlkvc=nlkvc, alk=alk, lwk=lwk, alloclen=alloclen, alloclan=alloclan, allocalk=allocalk)
})

# factor to factor
f2f <- function(x){
	x <- as.character(x)
	x <- gsub(" ", "", x)
	as.factor(x)
}

# fat= fullfill allocation tables
setGeneric("fat", function(object, ...){
	standardGeneric("fat")
	}
)

setMethod("fat", signature("data.frame"), function(object, ...){
	df0 <- unique(object)
	df0 <- cbind(df0, df0)
	names(df0) <- c("time", "space", "tech", "alloctime", "allocspace", "alloctech")	
	df0
})

setMethod("fat", signature("card"), function(object, ...){
	object@alloclen <- fat(object@nlkvc[,1:3])
	object@alloclan <- fat(object@Wk[,1:3])
	object@allocalk <- fat(object@alk[,1:3])
	object	
})

# consolidate alk
calk <- function(object, ...){
	df0 <- merge(object@alk, object@allocalk, all=T)
	aggregate(df0$freq, as.list(df0[,-c(1:3,8,9)]), sum, na.rm=T)
}

# consolidate lengths
clen <- function(object, ...){
	df0 <- merge(object@nlkvc, object@alloclen, all=T)
	df0 <- aggregate(df0$freq, as.list(df0[,-c(1:3,9)]), sum, na.rm=T)

	df1 <- merge(object@wkvc, object@alloclen, all=T)
	df1 <- aggregate(df1$wt, as.list(df0[,-c(1:3,9)]), sum, na.rm=T)

	df2 <- merge(object@Wkvc, object@alloclen, all=T)
	df2 <- aggregate(df0$wt, as.list(df0[,-c(1:3,9)]), sum, na.rm=T)
}

# consolidate landings
calk <- function(object, ...){
	df0 <- merge(object@Wk, object@alloclan, all=T)
	aggregate(df0$wt, as.list(df0[,-c(1:3,9)]), sum, na.rm=T)
}

