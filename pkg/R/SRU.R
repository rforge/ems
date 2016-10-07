#' Standardized Resource Use (SRU)
#'
#' @description Calculates the standardized resource use for ICUs with information about its patients.
#'
#'  \code{plot.SRU} Plots a SMR versus SRU scatter plot with your medians and tertiles.
#'
#'  \code{print.SRU} Prints a object of class 'SRU'.
#'
#' @param prob Death individual predictions (ranging from 0 to 1) in a vector.
#' @param death Observed death (Must be coded as 0 or 1. 1 for death).
#' @param unit ICU where the patient is admitted.
#' @param los Observed length of stay.
#' @param los.esp Estimated length of stay.
#' @param class Patient's severity range.
#' @param score Acute Physiology Score.
#' @param plot logical; If TRUE plots a SMR versus SRU scatter plot.
#' @param type Way to calculate SRU. If 1, does it as the article (default).
#' @param digits,digits2 Integer indicating the number of decimal places to be used in the output.
#' @param originals logical; If TRUE uses the severity classes and average days as the original article.
#' @param x  For \code{prin t.SRU}, an object of class 'SRU'.
#' @param xlim,ylim Limits of x and y axis for \code{plot.SRU}.
#' @param xlab,ylab Labels of x and y axis for \code{plot.SRU}.
#' @param points.arg List of arguments passed to \code{\link[graphics]{points}} for plotting points correponding to units' SMR and SRU for \code{plot.SRU}.
#' @param med.arg List of arguments passed to \code{\link[graphics]{abline}} for plotting lines corresponding to SRU and SMR medians for \code{plot.SRU}.
#' @param tert.arg List of arguments passed to \code{\link[graphics]{abline}} for plotting lines corresponding to SRU and SMR tertiles for \code{plot.SRU}.
#' @param auto.legend logical; If TRUE, prints a legend with \code{leg.arg} argumentes for \code{plot.SRU}.
#' @param leg.arg List of arguments passed to \code{\link[graphics]{legend}} for plotting legends corresponding to SRU and SMR medians and tertiles for \code{plot.SRU}.
#' @param ... Arguments to be passed to methods, such as \code{\link[graphics]{graphical parameters}} (see \code{\link[graphics]{par}}).
#'
#' @return Two tables: one with information about severity classes, and another with information about ICUs classified as Most Efficient or Least Efficient.
#' \itemize{
#' \item \code{Sev} Severity class.
#' \item\code{Total} Total of patients.
#' \item \code{Surv} Total of survivors.
#' \item \code{Total.LOS} Total length of stay (days).
#' \item \code{AvDays} Average days to produce a survivor.
#' \item \code{N.Unit} Quantity of ICUs.
#' \item \code{N.Pat} Quantity of patients.
#' \item\code{SMR} Standardized Mortality Ratio Mean (standard deviation) - see \code{\link{SMR}}.
#' \item \code{SRU} Standardized Resource Use Mean (standard deviation).
#' }
#'
#' Most Efficient ICUs have SRU,SMR < median. Least Efficient ICUs have SRU,SMR > median.
#'
#' @seealso \code{\link{SMR}}, \code{\link{reclass}}
#'
#' @author Lunna Borges and Pedro Brasil
#'
#' @examples
#'
#' # Loading the dataset
#' data(icu)
#'
#' days = seq(1,100)
#'
#' corte = cut_in(icu$Saps3Points,icu$los,icu$UnitDischargeName,icu$Unit,days,exc.ICU=TRUE)
#'
#' icu$class=cut(icu$Saps3Points,breaks=corte,include.lowest = TRUE)
#'
#' # Removing data with inapropriate values
#' icu <- icu[-which(icu$los < 0 ),]
#'
#' # Estimating the SRU
#' x <- SRU(prob = icu$Saps3DeathProbabilityStandardEquation,
#' death = icu$UnitDischargeName,unit = icu$Unit,
#' los = icu$los, score = icu$Saps3Points,
#' originals = TRUE, type = 1, plot = FALSE);x
#'
#' plot(x)
#'
#' # To see the units rankings and individual SMR and SRU, ordering by it SRU
#' x$rates[order(x$rates$sru),]
#'
#' # SRU with diferent severity classes created by cut_in function
#'y <- SRU(prob = icu$Saps3DeathProbabilityStandardEquation,
#'death = icu$UnitDischargeName, unit = icu$Unit,
#'los = icu$los, score = icu$Saps3Points,
#'originals = FALSE, type = 1, plot = FALSE, class = icu$class)
#' y
#'
#' rm(x, icu, corte)
#' @references
#' Rothen HU, Stricker K, Einfalt J, Bauer P, Metnitz PGH, Moreno RP, Takala J (2007) Variability in outcome and resource use in intensive care units. Intensive Care Med 33:1329-1336
#' @import stats
#' @import graphics
#' @export

SRU <- function(prob, death, unit, los, los.esp, class, score, plot = FALSE, type = 1, digits = 2, digits2 = 5, originals = FALSE){
  if(length(which(is.na(prob) == TRUE))){
    stop("Prob must not have any NA value.")
  }
  if(any(min(prob) <0 | max(prob) > 1)){
    stop("The individual predicted death must range from 0 to 1.")
  }
  if(length(which(is.na(death) == TRUE))){
    stop("Death must not have any NA value.")
  }
  if(all(death != 0 & death != 1)){
    stop("Observed death variable must be coded as 0 and 1.")
  }
  if(!is.factor(unit)){
    stop("Unit must be a factor.")
  }
  if(length(which(is.na(los) == TRUE))){
    stop("Los must not have any NA value.")
  }
  if(!is.numeric(los)){
    stop("LOS must be numeric.")
  }
  if (any(los < 0)){
    stop("There is at least one negative LOS. It must be a positive number.")
  }
  if(plot != TRUE && plot != FALSE){
    stop("Plot must be either 'TRUE' or 'FALSE'.")
  }
  if(type != 1 && type != 2){
    stop("Type must be either 1 or 2.")
  }
  if(type == 2){
    if(length(which(is.na(los.esp) == TRUE))){
      stop("Los.esp must not have any NA value.")
    }
    if(!is.numeric(los.esp)){
      stop("Expected LOS must be numeric.")
    }
    if (any(los.esp < 0)){
      stop("There is at least one negative expected LOS. It must be a positive number.")
    }
  }
  if(originals != TRUE && originals != FALSE){
    stop("Originals must be either 'TRUE' or 'FALSE'.")
  }
  if (originals == TRUE){
    if(length(which(is.na(score) == TRUE))){
      stop("Score must not have any NA value.")
    }
    if(!is.numeric(score)){
      stop("Score must be numeric.")
    }
    class=cut(score, breaks=c(min(score),24,34,44,54,64,74,84,94,max(score)), include.lowest = T)
  }
  if (originals == FALSE){
    if(!is.factor(class)){
      stop("Class must be a factor.")
    }
  }
  if(type == 1){dt = data.frame(prob, death, unit, los, class)}
  if(type == 2){dt = data.frame(prob, death, unit, los, los.esp, class)}
  unit_death <- table(dt$unit, dt$death)
  exc <- rownames(unit_death)[which(unit_death[,1] == 0)]
  if (!is.null(exc) & length(exc) > 0){
    dt=dt[-which(dt$unit %in% exc),]
    dt <- droplevels(dt)
    unit_death <- table(dt$unit,dt$death)
    warning(paste(c("The following units were excluded due to absence of survivals:", exc), collapse = " "))	}

  cla_los <- aggregate(dt$los, by = list(Class = dt$class), FUN = sum)
  sum_los <- cla_los[2]	        	#SOMA LOS-ICU POR ESTRATO

  cla_dea <- table(dt$class, dt$death)
  surv <- cla_dea[,1]			#N? DE SOBREVIVENTES POR ESTRATO
  total =  cla_dea[,1] + cla_dea[,2]		#TOTAL DE PACIENTES POR ESTRATO

  if (originals == FALSE){
    average_days <- sum_los / surv 	#N? M?DIO DE RECURSOS USADO POR PACIENTE,POR ESTRATO
  }
  if (originals == TRUE){
    average_days <- matrix(c(2.3,3.2,4.3,7.2,11,16.6,22.2,29.4,39), ncol = 1)
  }
  if (type == 1){
    # unit_death[,1]					#N? DE SOBREVIVENTES POR UNIDADE
    unit_class <- data.frame(dt$unit,dt$class, dt$death)
    unit_class <- unit_class[-which(unit_class$dt.death==1),]
    unit_class <- table(unit_class)
    unit_class <- matrix(unit_class, nrow(unit_death), nrow(average_days))	#N? DE SOBREVIVENTE POR UNIDADE EM CADA ESTRATO
    #rownames(unit_class)=rownames(unit_death); colnames(unit_class)=rownames(cla_dea)
    A <- matrix(average_days[,1], nrow(unit_death), nrow(average_days), byrow=T) #MATRIZ RECURSOS POR ESTRATO
    rec_unit_class <- A*unit_class		#USO DE RECURSO ESPERADO PARA PRODUZIR N? DE SOBREV POR UNIDADE E ESTRATO
    LOS_ICU_esp <- apply(rec_unit_class, MARGIN=1, FUN = sum)		#LOS_ICU ESPERADO
  }
  if (type == 2){
    LOS_ICU_esp <- aggregate(dt$los.esp, by <- list(Class <- dt$unit), FUN = sum)[,2]
  }

  LOS_ICU_obs <- aggregate(dt$los, by = list(Class = dt$unit), FUN = sum)[,2]		#LOS_ICU TOTAL OBSRVADO POR UNIDADE
  sru <- LOS_ICU_obs / LOS_ICU_esp					#SRU POR UNIDADE
  # unit_death[,2]			#N?MERO DE ?BITOS POR UNIDADE
  B <- aggregate(dt$prob,by=list(Class=dt$unit),FUN=sum)[,2]	#SOMA DA PROBAB DE MORTE POR UNIDADE
  smr <- unit_death[,2] / B		#SMR POR UNIDADE
  M1 <- median(sru);	M2 <- median(smr)
  Q1 <- quantile(sru, prob = c(.33,.66))		#TERCIS
  Q2 <- quantile(smr, prob = c(.33,.66))
  rates <- data.frame(sru, smr, unit = labels(smr))				#DEFINI??O DO QUADRANTE POR UNIDADE
  rates$group <- NA
  rates$group[which(rates[,1] < M1 & rates[,2] < M2)] <- "ME"
  rates$group[which(rates[,1] >= M1 & rates[,2] >= M2)] <- "LE"
  rates$group[which(rates[,1] >= M1 & rates[,2] < M2)] <- "OVER"
  rates$group[which(rates[,1] < M1 & rates[,2] >= M2)] <- "UNDER"

  rates.ef <- rates[which(rates$group == "ME" | rates$group == "LE"),]  #UNIDADES CLASSIFICADAS COMO "ME" OU "LE"
  rates.ef$LT <- NA
  rates.ef$Menor.Median <- NA
  rates.ef$Maior.Median <- NA
  rates.ef$HT <- NA
  rates.ef$unit <- as.factor(rownames(rates.ef))

  rates.ef$LT <- ifelse (rates.ef[,1] < Q1[1] & rates.ef[,2] < Q2[1], TRUE, FALSE)
  rates.ef$HT <- ifelse (rates.ef[,1] > Q1[2] & rates.ef[,2] > Q2[2], TRUE, FALSE)
  rates.ef$Menor.Median <- ifelse(rates.ef$group == "ME", TRUE, FALSE)
  rates.ef$Maior.Median <- ifelse(rates.ef$group == "LE", TRUE, FALSE)

  quant_unit <- c(length(rates.ef$unit[which(rates.ef$LT == TRUE)]),
                  length(rates.ef$unit[which(rates.ef$Menor.Median == TRUE)]),
                  length(rates.ef$unit[which(rates.ef$Maior.Median == TRUE)]),
                  length(rates.ef$unit[which(rates.ef$HT == TRUE)]),
                  length(rates.ef[,1]))		#QUANTIDADE DE UNIDADES POR CLASSE

  sru.mean <- c(mean(rates.ef$sru[which(rates.ef$LT == TRUE)]),				#M?DIA DO SRU EM CADA CLASSE
                mean(rates.ef$sru[which(rates.ef$Menor.Median == TRUE)]),
                mean(rates.ef$sru[which(rates.ef$Maior.Median == TRUE)]),
                mean(rates.ef$sru[which(rates.ef$HT == TRUE)]),
                mean(rates.ef$sru))

  sru.sd <- c(sd(rates.ef$sru[which(rates.ef$LT == TRUE)]),					#DESVIO PADR?O DO SRU EM CADA CLASSE
              sd(rates.ef$sru[which(rates.ef$Menor.Median == TRUE)]),
              sd(rates.ef$sru[which(rates.ef$Maior.Median == TRUE)]),
              sd(rates.ef$sru[which(rates.ef$HT == TRUE)]),
              sd(rates.ef$sru))

  smr.mean <- c(mean(rates.ef$smr[which(rates.ef$LT == TRUE)]),				#M?DIA DO SMR EM CADA CLASSE
                mean(rates.ef$smr[which(rates.ef$Menor.Median == TRUE)]),
                mean(rates.ef$smr[which(rates.ef$Maior.Median == TRUE)]),
                mean(rates.ef$smr[which(rates.ef$HT == TRUE)]),
                mean(rates.ef$smr))

  smr.sd <- c(sd(rates.ef$smr[which(rates.ef$LT == TRUE)]),					#DESVIO PADR?O DO SMR POR CLASSE
              sd(rates.ef$smr[which(rates.ef$Menor.Median == TRUE)]),
              sd(rates.ef$smr[which(rates.ef$Maior.Median == TRUE)]),
              sd(rates.ef$smr[which(rates.ef$HT == TRUE)]),
              sd(rates.ef$smr))

  pacient_total <- sum(table(dt$unit)[match(rates.ef$unit, as.factor(names(table(dt$unit))))])	 							#N TOTAL DE PACIENTES
  pacient_LT <- sum(table(dt$unit)[match(rates.ef$unit[which(rates.ef$LT == TRUE)], as.factor(names(table(dt$unit))))])				#N TOTAL DE PACIENTES EM LT
  pacient_Menor.Median <- sum(table(dt$unit)[match(rates.ef$unit[which(rates.ef$Menor.Median == TRUE)], as.factor(names(table(dt$unit))))])	#N TOTAL DE PACIENTES EM <MEDIAN
  pacient_Maior.Median <- sum(table(dt$unit)[match(rates.ef$unit[which(rates.ef$Maior.Median==TRUE)], as.factor(names(table(dt$unit))))])	#N TOTAL DE PACIENTES EM >MEDIAN
  pacient_HT <- sum(table(dt$unit)[match(rates.ef$unit[which(rates.ef$HT == TRUE)], as.factor(names(table(dt$unit))))])				#N TOTAL DE PACIENTES EM HT

  n.pacients <- c(pacient_LT, pacient_Menor.Median, pacient_Maior.Median, pacient_HT, pacient_total)

  unit_admissions = apply(unit_death,1,sum)

  output <- list(
    LOS.surv = data.frame(row.names=NULL,labels(total), total, surv, round(as.vector(sum_los), digits2), round(average_days,digits2)),
    estim.eff = data.frame(N.Unit=quant_unit, N.Pat = n.pacients, SMR = paste(format(smr.mean,digits = digits, nsmall = digits), " ",
                                                                              "(", format(smr.sd,digits = digits, nsmall = digits),")", sep=""), SRU=paste(format(sru.mean, digits = digits, nsmall = digits)," ","(",format(sru.sd,digits = digits,nsmall = digits),")", sep = "")),
    rates = rates,
    med = c(M1,M2),
    tert = c(Q1,Q2),
    LOS_obs = LOS_ICU_obs,
    LOS_esp = LOS_ICU_esp,
    ratesef = which(rates$group == "ME" | rates$group == "LE"),
    totalICU = nrow(unit_death), totalAd = unit_admissions
  )
  colnames(output$LOS.surv) <- c("Sev","Total","Surv","Total.LOS","AvDays")
  rownames(output$estim.eff) <- c("Low.Tert","<Median",">Median","High.Tert","All.Units")

  class(output) <- "SRU"

  if (plot == TRUE){plot(output)}

  output
}

#' @rdname SRU
#' @export

print.SRU <- function(x, ...){
  print(x$LOS.surv, ...)
  cat("--------------------------------------------------------\n")
  if (length(x$ratesef) > 1){
    print(x$estim.eff, ...)
  } else{
    cat("There are", as.character(length(x$rates[which(x$rates$group == "ME")])), "Most Efficient and", as.character(length(x$rates[which(x$rates$group == "LE")])), "Least Efficient ICUs in",x$totalICU, "ICUs.", sep= " ")
  }
}

#' @rdname SRU
#' @export
plot.SRU <- function(x, ..., xlim = range(x$rates[,2]), ylim = range(x$rates[,1]), xlab = "SMR", ylab = "SRU", points.arg = list(pch = 21, col = "white", bg = "cadetblue3",cex=1.5), med.arg = list(col="dodgerblue4",lwd = 2,lty = 1), tert.arg = list(col = "darkorange2", lty = 2, lwd = 1), auto.legend = TRUE, leg.arg = list(x = "top", bty = "n", xpd = NA, inset = -.15, ncol = 2)){
  plot(0, 0, ..., xlim = xlim, ylim = ylim, type = 'n')
  med.arg$v <- x$med[2]
  med.arg$h <- x$med[1]
  tert.arg$h <- x$tert[1:2]
  tert.arg$v <- x$tert[3:4]
  points.arg$x <- x$rates[,c(2,1)]
  do.call(abline, med.arg)
  do.call(abline, tert.arg)
  do.call(points, points.arg)
  if(auto.legend){
    leg.arg$legend <- c("Median", "Tertile")
    leg.arg$col <- c(med.arg$col, tert.arg$col)
    leg.arg$lty <- c(med.arg$lty, tert.arg$lty)
    leg.arg$lwd <- c(med.arg$lwd, tert.arg$lwd)
    do.call(legend, leg.arg)
  }
}
