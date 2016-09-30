#' cut.in()
#'
#' To find cut off points to define severity classes to use in function \code{\link{SRU}} and calculate average days to discharged surviving patients.
#'
#' @param escore Acute Physiology Score.
#' @param los Observed length of stay.
#' @param death Observed death (must be coded as 0 or 1. 1 for death).
#' @param unit The ICU where a patient was admitted.
#' @param days Vector with days which one wants to average days range in.
#' @param min The minimum quantity of patients  in each severity class (default \code{min} = 200)
#' @param exc.ICU logical; If TRUE, ICUs without surviving patients are ignored.
#' @param complete logical; If TRUE, shows additional information about severity classes.
#' @param digits Integer indicating the number of decimal places to be used in the complete output..
#' @return A vector with cut off severity points. Use function \code{\link[base]{cut}} to apply them to the score punctuation and classify your patients data.
#' @examples
#' data(icu)
#' days = seq(1,100)
#' corte = cut.in(icu$Saps3Points,icu$los,icu$UnitDischargeName,icu$Unit,days,exc.ICU=TRUE)
#' icu$class=cut(icu$Saps3Points,breaks=corte,include.lowest = T)
#' SRU(prob=icu$Saps3DeathProbabilityStandardEquation,death=icu$UnitDischargeName,unit=icu$Unit,los=icu$los,originals=F,type = 1,plot=F,score=icu$Saps3Points,class=icu$class)
#' @export

cut.in <- function (escore, los, death, unit, days, min = 200, exc.ICU = TRUE, complete = FALSE, digits = 5){
	if(!is.numeric(escore)){
		stop("Escore must be numeric.")}
	if(!is.numeric(los)){
		stop("LOS must be numeric.")}
	if(any(as.factor(death) != 0 & as.factor(death) != 1)){
		stop("Observed death variable must be coded as 0 and 1.")}
	if (!is.numeric(days)){
		stop("Days to be tested must be numeric.")}
	if(!is.factor(unit)){
		stop("Unit must be a factor.")}
	if(exc.ICU != TRUE && exc.ICU != FALSE){
		stop("Exc.ICU must be either 'TRUE' or 'FALSE'.")}
	if(complete != TRUE && complete != FALSE){
		stop("Complete must be either 'TRUE' or 'FALSE'.")}

	dt <- data.frame(escore, los, death, unit)

	if (exc.ICU == TRUE){
		unit_death <- table(dt$unit, dt$death)
		exc <- rownames(unit_death)[which(unit_death[,1] == 0)]

		if (!is.null(exc) & length(exc) > 0){
			dt <- dt[-which(dt$unit %in% exc),]			#EXCLUI AS UNIDADES SEM SOBREVIVENTES
			dt <- droplevels(dt)
			warning(paste(c("The following units were excluded due to absence of survivals:", exc), collapse=" "))
		}
	}
	un <- sort(unique(dt$escore))
	breaks <- NULL; avdays <- c(); pt.corte <- c(); b <- c(); d <- c(); da <- c(); tail <- c()
	j <- 1;	w <- days[1]

	for (i in 2:length(un)) {
		breaks[j:i] <- un[j:i]
		cond <- which(dt$escore >= breaks[j] & dt$escore < breaks[i])

		if (length(dt$escore[cond]) >= min) {
			sum_los <- sum(dt$los[cond])		#SOMA DO TEMPO DE PERMAN?NCIA
			surv <- (length(dt$death[cond]) - sum(dt$death[cond]))		#N? DE SOBREVIVENTES
			avdays <- append(avdays, sum_los / surv) 		#N? M?DIO DE RECURSOS USADOS PARA TER RESPECTIVO N?MERO DE SOBREVIVENTES
			b <- append(b,breaks[i-1])		#POSS?VEIS PONTOS DE CORTE

			if (tail(avdays,1) > w){
				tail <- append(tail, tail(avdays,1))
				da <- append(da, w)
				pt.corte <- append(pt.corte, b[which.min(abs(avdays - w))])
				d <- append(d, avdays[which.min(abs(avdays - w))])	#DIAS M?DIOS DE RECURSO
				w <- days[match(which(days > tail(d,1) & days > w), days)][1]
				avdays <- c()
				b <- c()
				i <- which(un == tail(pt.corte, 1) + 1)
				j <- which(un == tail(pt.corte, 1) + 1)
			}
			i <- i + 1
		} else {i <- i + 1}
	}

	corte <- c(min(escore), pt.corte, max(escore))
	L <- c()
	lowest <- min(escore) - 1

	for (k in 1:length(corte)){			#TOTAL DE PACIENTES POR ESTRATO
		cond2 <- length(which(dt$escore > lowest & dt$escore <= corte[k + 1]))
		lowest <- corte[k + 1]
		L <- append(L, cond2)
	}
	L=L[-length(L)]

	if (L[length(L)] < min){ 	#JUNTA AS ?LTIMAS CLASSES, SE A ?LTIMA TIVER MENOS QUE O N?MERO M?NIMO DE PACIENTES
		pt.corte <- pt.corte[-length(pt.corte)]
		da <- da[-length(da)]
		tail <- tail[-length(tail)]
		d <- d[-length(d)]
		L2 <- L[-length(L)]
		corte <- c(min(escore), pt.corte, max(escore))
	}
	if (complete == TRUE){
		cond3 <- which(dt$escore > corte[length(corte) - 1] & dt$escore <= max(dt$escore))
		sum_los <- sum(dt$los[cond3])
		surv <- (length(dt$death[cond3]) - sum(dt$death[cond3]))
		high.avday <- sum_los / surv

		if (L[length(L)] < min){
			output <- data.frame("Days"=da, "Tail" = round(tail,digits), "Corte" = pt.corte, "AvDays" = round(d,digits), "Total.Pacients" = L2[-length(L2)])
			highest <- c("-", "-", max(dt$escore), round(high.avday, digits), sum(L[length(L)], L[length(L) - 1]))
			output <- rbind(output, highest)
		}else{
			output <- data.frame("Days" = da, "Tail" = round(tail, digits), "Corte" = pt.corte, "AvDays" = round(d,digits), "Total.Pacients" = L[-length(L)])
			highest <- c("-", "-", max(dt$escore), round(high.avday, digits), L[length(L)])
			output <- rbind(output, highest)
		}
	} else {corte}

}

