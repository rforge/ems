#' cut_in()
#'
#' @rdname SRU
#'
#' @description \code{cut_in} is used to find cut off points to define severity classes to use in function \code{SRU} and calculate average days to discharged surviving patients.
#' @param days For \code{cut_in}. Vector with days which one wants to average days range in.
#' @param min For \code{cut_in}. The minimum quantity of patients  in each severity class (default \code{min} = 200)
#' @param exc.ICU logical; For \code{cut_in}. If TRUE, ICUs without surviving patients are ignored.
#' @param complete logical; For \code{cut_in}. If TRUE, shows additional information about severity classes.
#' @return \code{cut_in} returns a vector with cut off severity points. Use function \code{\link[base]{cut}} to apply them to the score punctuation and classify your patients data.
#' @import utils
#' @export

cut_in <- function (score, los, death, unit, days, min = 200, exc.ICU = TRUE, complete = FALSE, digits = 5){
	if(!is.numeric(score)){
		stop("score must be numeric.")}
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

	dt <- data.frame(score, los, death, unit)

	if (exc.ICU == TRUE){
		unit_death <- table(dt$unit, dt$death)
		exc <- rownames(unit_death)[which(unit_death[,1] == 0)]

		if (!is.null(exc) & length(exc) > 0){
			dt <- dt[-which(dt$unit %in% exc),]			#EXCLUI AS UNIDADES SEM SOBREVIVENTES
			dt <- droplevels(dt)
			warning(paste(c("The following units were excluded due to absence of survivals:", exc), collapse=" "))
		}
	}
	un <- sort(unique(dt$score))
	breaks <- NULL; avdays <- c(); pt.corte <- c(); b <- c(); d <- c(); da <- c(); tail <- c()
	j <- 1;	w <- days[1]

	for (i in 2:length(un)) {
		breaks[j:i] <- un[j:i]
		cond <- which(dt$score >= breaks[j] & dt$score < breaks[i])

		if (length(dt$score[cond]) >= min) {
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

	corte <- c(min(score), pt.corte, max(score))
	L <- c()
	lowest <- min(score) - 1

	for (k in 1:length(corte)){			#TOTAL DE PACIENTES POR ESTRATO
		cond2 <- length(which(dt$score > lowest & dt$score <= corte[k + 1]))
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
		corte <- c(min(score), pt.corte, max(score))
	}
	if (complete == TRUE){
		cond3 <- which(dt$score > corte[length(corte) - 1] & dt$score <= max(dt$score))
		sum_los <- sum(dt$los[cond3])
		surv <- (length(dt$death[cond3]) - sum(dt$death[cond3]))
		high.avday <- sum_los / surv

		if (L[length(L)] < min){
			output <- data.frame("Days"=da, "Tail" = round(tail,digits), "Corte" = pt.corte, "AvDays" = round(d,digits), "Total.Pacients" = L2[-length(L2)])
			highest <- c("-", "-", max(dt$score), round(high.avday, digits), sum(L[length(L)], L[length(L) - 1]))
			output <- rbind(output, highest)
		}else{
			output <- data.frame("Days" = da, "Tail" = round(tail, digits), "Corte" = pt.corte, "AvDays" = round(d,digits), "Total.Pacients" = L[-length(L)])
			highest <- c("-", "-", max(dt$score), round(high.avday, digits), L[length(L)])
			output <- rbind(output, highest)
		}
	} else {corte}

}

