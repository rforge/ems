#' Funnel Plot For Standardized Rates
#'
#' @description \code{rateFunnel} plots a standardized rate y versus its expected death or volume value for several units. Funnel plots are usefull for identifing units which are "in control" or not.
#'
#' @param unit The unit names.
#' @param y Standardized rate vector, as SMR or SRU, accordind to \code{y.type}. It's also called "indicator".
#' @param n Volume of cases, admissions, for each unit. Used when \code{direct = TRUE}
#' @param o Observed death vector. Accepted values are 0 (absence) or 1 (presence). Used when \code{direct = TRUE}.
#' @param e Expected death vector. Used when \code{direct = FALSE}.
#' @param y.type The indicator type. It can be SMR or SRU.
#' @param p Confidence level vector. It will return a confidence interval for all vector components. The default is 2 and 3 standard deviations (p = c(.95, 998).
#' @param theta Target value which specifies the desired expectation for institutions considered "in control".
#' @param direct Logical; If \code{TRUE}, we assume the rates are reported as a rate per (say) 1000 individuals, and that the rate has been transformed to a proportion y between 0 and 1. The measure of the associated error may be reported in the size of population \code{n} (CI is made a binomial approximation). If \code{FALSE}, it may be reported in the population expected death \code{e} (CI is made a poisson approximation). See details.
#' @param method There are two kind of approximations, as discussed in \code{direct} parameter. Inside them, there are two options:  to make the CI from the exact distribuition (binomial or poisson) or from de normal distribution. So, here one could choose between \code{normal} or \code{exact} (default). See details.
#' @param col Especification vector for the CI lines colors. Must have same length of \code{p} + 1 for the target line in the last position.
#' @param lwd The lines width, a positive number. It's the same for ao lines in the plot. See \code{\link[graphics]{par}}.
#' @param lty The CI lines types. See \code{\link[graphics]{par}}.
#' @param bty A character string which determined the type of \code{\link[graphics]{box}} which is drawn about plots. See \code{\link[graphics]{par}}.
#' @param pch Either an integer specifying a symbol or a single character to be used as the default in plotting points. See \code{\link[graphics]{points}} for possible values and their interpretation. Note that only integers and single-character strings can be set as a graphics parameter (and not NA nor NULL).
#' @param bg The color to be used for the background of the points for \code{pch = 21}. See \code{\link[graphics]{par}}.
#' @param pt.col Especification vector for the points colors.
#' @param pt.cex A numerical value giving the amount by which plotting points should be magnified relative to the default.  See \code{\link[graphics]{par}}.
#' @param auto.legend Logical; If \code{TRUE}, prints a legend with default arguments.
#' @param printUnits Logical; If \code{TRUE}, the units are identified in the plot and printed in de console. The numbers in the plot correspond to the data.frame printed in the console.
#' @param text.cex Like \code{pt.cex}, but for the texts numbers correspondents to the units.
#' @param text.pos a position specifier for numbers that correspond to the units in the plot. Values of 1, 2, 3 and 4, respectively indicate positions below, to the left of, above and to the right of the points.
#' @param auto.xlab Logical; if \code{TRUE}, the plot is returned with default xlab.
#' @param xlab,ylab A title for the x and y axis. To change xlab one must set \code{auto.xlab = FALSE}. See \code{\link[graphics]{title}}
#' @param xlim,ylim Numeric vector giving the x and y coordinates ranges.
#' @param plot Logical; If \code{TRUE}, plots the correspondent graphic.
#' @param myunits A character vector with the unit names which one would like to benchmark among all units.
#' @param mypts.col The color passed to \code{\link[graphics]{points}} for the the units specified in \code{myunits}.
#' @param overdispersion Logical; If \code{TRUE}, introduces an multiplicative over-dispersion factor phi that will inflate the normal CI null variance. See details.
#' @param digits Integer indicating the number of decimals to be used in the output.
#' @param ... Further arguments passed to \code{\link[graphics]{plot}}.
#'
#' @details
#' To choose the \code{direct} argument, one should pay attention if one wants to use a Direct Standardized Rate or a Indirect Standardized Rate. If direct, we assume the rate is reported as a rate per (say) 1000 individuals, then it is treated as a proportion. If indirect, it is a cross-sectional data that leads to a standardized event ratio.
#'
#' In many circumstances we can assume an exact or approximate normal distribution for the data. Using the \code{type} argument, one could choose between \code{exact} or  \code{normal}. For direct standardized rates, the exact distribuition is binomial and for indirect standardized rates, the exact distribuition is poisson. Assume rho is the precision parameter (volume, for direct rates; expected value, for indirect rates). For rho > 100 the normal and exact curves almost coincide. So, one could perfectly use  normal approximation if ones data parameter precision is greater than 100, in general.
#'
#' If \code{overdispersion = TRUE}, the normal CI ins inflated by a overdispersion parameter phi. It is kind of arbitrary and usefull in high-volume outcome measures, when the large majority of institutions lie outside the funnel, casting doubt on the appropriateness of the limits.
#'
#' phi = (1/total) * sum((y - theta) ^ 2 * e)/theta
#'
#' var(y|theta,n) = (phi * g(theta))/e
#'
#' @return A list with usefull information used in \code{\link{funnel}}.
#' @references
#' Spiegelhalter, David J. "Funnel plots for comparing institutional performance." Statistics in medicine 24.8 (2005): 1185-1202.
#' @seealso \code{\link{SMR}}, \code{\link{SRU}}
#' @author Lunna Borges and Pedro Brasil
#' @import stats
#' @import graphics
#' @export


rateFunnel <- function(unit, y, n, o, e, y.type = c("SMR","SRU"), p = c(.95,.998), theta = 1, method = c("exact","normal"), direct = FALSE, ..., col = c("skyblue4","skyblue2","snow4"), lwd = 2, lty = c(2,6,1), bty = "n", pch = 21, pt.col = "white", bg = "orange", pt.cex = 1.5, auto.legend = TRUE, printUnits = FALSE, text.cex = 0.7, text.pos = NULL, auto.xlab = TRUE, xlab = c("Volume of cases","Expected values"), ylab = y.type[1], xlim = c(0, max(rho)), ylim = c(min(lowerCI[[which(p == max(p))]]), max(upperCI[[which(p == max(p))]])), plot = FALSE, myunits = NULL, mypts.col = "darkblue", overdispersion = FALSE, digits = 5){

  if(!is.factor(unit)){stop("Unit must be a factor.")}
  if (!is.numeric(y)){stop("y must be numeric.")}
  if (!is.logical(direct)){stop("direct must be TRUE or FALSE.")}
  if (direct){
    if (!is.numeric(n)){stop("n must be numeric.")}
    if (!is.numeric(o)){stop("o must be numeric.")}
  } else{
    if (!is.numeric(e)){stop("e must be numeric.")}
  }
  if (!is.numeric(theta)){stop("theta must be numeric.")}
  if (!is.numeric(p)){stop("p must be a numeric vector.")}
  if (!is.vector(p)){stop("p must be a vector.")}
  if (method[1] != "normal" && method[1] != "exact"){stop("method must be either 'normal' or 'exact'.")}
  if (length(col) != length(p)+1){stop("col must have same length of p + 1 for the target line color in the last position")}
  # if (length(lty) != length(p)){stop("p and lty must have same length")}
  if (!is.logical(auto.legend)){stop("auto.legend must be TRUE or FALSE.")}
  if (y.type[1] != "SMR" && y.type[1] != "SRU"){stop("y.type must be either 'SMR' or 'SRU'.")}
  if (!is.logical(printUnits)){stop("printUnits must be TRUE or FALSE.")}
  if (!is.logical(plot)){stop("plot must be TRUE or FALSE.")}
  if (!is.logical(overdispersion)){stop("overdispersion must be TRUE or FALSE.")}

  if (direct){
    rates.table <- data.frame(unit, Rate = y, Admissions = n, Observed = o)
    thirdcolname <-  "Admissions"
  } else {
    rates.table <- data.frame(unit, Rate = y, Expected = e, Observed = o)
    thirdcolname <- "Expected"
  }

  upperCI <- list()
  lowerCI <- list()
  ylowCI <- list()
  yuppCI <- list()
  lowOUT <- list()
  uppOUT <- list()
  outofcontrol <- list()
  outcolname <- c()

  if (direct){
    rates.table <-  rates.table[order(rates.table$Admissions),] # ordering by total of admissions
    smr <- rates.table$Rate
    admissions <- rates.table$Admissions
    observed <- rates.table$Observed
    admissionsRange <- seq(1, max(admissions))
    observedRange <- seq(1, max(observed),length.out = length(admissionsRange))
    rho <- admissions
    unitnames <- data.frame(Unit = rates.table$unit)

    if (auto.xlab){ xlab = xlab[1] }

    # Exact formula using binomial approximation
    if (method[1] == "exact"){
      prob <- observedRange/admissionsRange
      for (i in 1:length(p)){
        rp <- qbinom(p[i], size = admissionsRange, prob)
        alpha <- (pbinom(rp, size = admissionsRange, prob) - p[i]) / ((pbinom(rp, size = admissionsRange, prob)) - pbinom(rp - 1, size = admissionsRange, prob))
        upperCI[[i]] <- theta + (rp - alpha) / admissionsRange
        lowerCI[[i]] <- theta - (rp - alpha) / admissionsRange
        ylowCI[[i]] <- lowerCI[[i]][which(admissionsRange %in% admissions == TRUE)]
        yuppCI[[i]] <- upperCI[[i]][which(admissionsRange %in% admissions == TRUE)]
        lowOUT[[i]] <- ifelse(smr < ylowCI[[i]],TRUE,FALSE)
        uppOUT[[i]]<- ifelse(smr > yuppCI[[i]], TRUE, FALSE)
        outofcontrol[[i]] <- ifelse(lowOUT[[i]] == TRUE | uppOUT[[i]] == TRUE, "OUT","-")
        outcolname[i] <- paste0(p[i]*100,"%CI")
      }
        rates.table <- cbind(rates.table, outofcontrol)
        x <- admissions; y <- smr; range <- admissionsRange
    }
    if (method[1] == "normal"){
      if (any(admissions < 100)){
        warning("Normal distribuition is not a good approximation for units whose has less than 100 admissions. There are ", length(which(admissions < 100)), " units with less than 100 admissions")
      }
      if (overdispersion){
        phi <- (1/nrow(rates.table)) * sum(((y - theta) ^ 2 * admissions)/(theta))
        for (i in 1:length(p)){
          zp <- qnorm(1 - (1 - p[i]) / 2)
          upperCI[[i]] <- theta + zp * sqrt(theta * phi / admissionsRange)
          lowerCI[[i]] <- theta - zp * sqrt(theta * phi / admissionsRange)
          ylowCI[[i]] <- lowerCI[[i]][which(admissionsRange %in% admissions == TRUE)]
          yuppCI[[i]] <- upperCI[[i]][which(admissionsRange %in% admissions == TRUE)]
          lowOUT[[i]] <- ifelse(smr < ylowCI[[i]],TRUE, FALSE)
          uppOUT[[i]] <- ifelse(smr > yuppCI[[i]], TRUE, FALSE)
          outofcontrol[[i]] <- ifelse(lowOUT[[i]] == TRUE | uppOUT[[i]] == TRUE, "OUT","-")
        }
      } else {
        for (i in 1:length(p)){
          zp <- qnorm(1 - (1 - p[i]) / 2)
          upperCI[[i]] <- theta + zp * sqrt(theta / admissionsRange)
          lowerCI[[i]] <- theta - zp * sqrt(theta / admissionsRange)
          ylowCI[[i]] <- lowerCI[[i]][which(admissionsRange %in% admissions == TRUE)]
          yuppCI[[i]] <- upperCI[[i]][which(admissionsRange %in% admissions == TRUE)]
          lowOUT[[i]] <- ifelse(smr < ylowCI[[i]],TRUE, FALSE)
          uppOUT[[i]] <- ifelse(smr > yuppCI[[i]], TRUE, FALSE)
          outofcontrol[[i]] <- ifelse(lowOUT[[i]] == TRUE | uppOUT[[i]] == TRUE, "OUT","-")
          outcolname[i] <- paste0(p[i]*100,"%CI")
        }
      }
        rates.table <- cbind(rates.table, outofcontrol)
        x <- admissions; y <- smr; range <- admissionsRange
    }
  } else {
    rates.table <-  rates.table[order(rates.table$Expected),] # ordering by expected death
    smr <- rates.table$Rate
    e <- rates.table$Expected
    expectedRange <- seq(1, max(e)) #using expected death as precision parameter
    rho <- e
    unitnames <- data.frame(Unit = rates.table$unit)
    if (auto.xlab){ xlab = xlab[2]}
  # "exact" formula using poisson approximation
    if (method[1] == "exact"){
      lambda <- theta*expectedRange
      for (i in 1:length(p)){
        rp <- qpois(p[i], lambda)
        alpha <- (ppois(rp, lambda) - p[i]) / (ppois(rp, lambda) - ppois(rp - 1, lambda))
        upperCI[[i]] <- theta + (rp - alpha)/expectedRange
        lowerCI[[i]] <- theta - (rp - alpha)/expectedRange
        ylowCI[[i]] <- lowerCI[[i]][which(expectedRange %in% trunc(e) == TRUE)]
        yuppCI[[i]] <- upperCI[[i]][which(expectedRange %in% trunc(e) == TRUE)]
        lowOUT[[i]] <- ifelse(smr < ylowCI[[i]],TRUE, FALSE)
        uppOUT[[i]] <- ifelse(smr > yuppCI[[i]], TRUE, FALSE)
        outofcontrol[[i]] <- ifelse(lowOUT[[i]] == TRUE | uppOUT[[i]] == TRUE, "OUT","-")
        outcolname[i] <- paste0(p[i]*100,"%CI")
      }
        rates.table <- cbind(rates.table, outofcontrol)
        x <- e; y <- smr; range <- expectedRange
    }
  # Normal approximation
    if (method[1] == "normal"){
      if (any(e < 100)){
        warning("Normal distribuition is not a good approximation for units whose has less than 100 expected values. There are ", length(which(e < 100)), " units with less than 100 expected values")
      }
      if (overdispersion){
        phi <- (1/nrow(rates.table)) * sum(((y - theta) ^ 2 * e)/(theta))
        for (i in 1:length(p)){
          zp <- qnorm(1 - (1 - p[i]) / 2)
          upperCI[[i]] <- theta + zp * sqrt(theta * phi / expectedRange)
          lowerCI[[i]] <- theta - zp * sqrt(theta * phi / expectedRange)
          ylowCI[[i]] <- lowerCI[[i]][which(expectedRange %in% trunc(e) == TRUE)]
          yuppCI[[i]] <- upperCI[[i]][which(expectedRange %in% trunc(e) == TRUE)]
          lowOUT[[i]] <- ifelse(smr < ylowCI[[i]],TRUE, FALSE)
          uppOUT[[i]] <- ifelse(smr > yuppCI[[i]], TRUE, FALSE)
          outofcontrol[[i]] <- ifelse(lowOUT[[i]] == TRUE | uppOUT[[i]] == TRUE, "OUT","-")
          outcolname[i] <- paste0(p[i]*100,"%CI")
        }
      } else {
      for (i in 1:length(p)){
          zp <- qnorm(1 - (1 - p[i]) / 2)
          upperCI[[i]] <- theta + zp * sqrt(theta/expectedRange)
          lowerCI[[i]] <- theta - zp * sqrt(theta/expectedRange)
          ylowCI[[i]] <- lowerCI[[i]][which(expectedRange %in% trunc(e) == TRUE)]
          yuppCI[[i]] <- upperCI[[i]][which(expectedRange %in% trunc(e) == TRUE)]
          lowOUT[[i]] <- ifelse(smr < ylowCI[[i]],TRUE, FALSE)
          uppOUT[[i]] <- ifelse(smr > yuppCI[[i]], TRUE, FALSE)
          outofcontrol[[i]] <- ifelse(lowOUT[[i]] == TRUE | uppOUT[[i]] == TRUE, "OUT","-")
        outcolname[i] <- paste0(p[i]*100,"%CI")
      }
    }
      rates.table <- cbind(rates.table, outofcontrol)
      x <- e; y <- smr; range <- expectedRange
  }
 }

  output <- list(x = x, y = y, theta = theta, range = range, tab = rates.table, upperCI = upperCI, lowerCI = lowerCI, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, myunits = myunits, p = p, unitnames = unitnames, printUnits = printUnits)
  colnames(output$tab) <- c("Unit", y.type[1], thirdcolname, "Observed", outcolname)
  rownames(output$tab) <- seq(1,nrow(output$tab))
  output$tab[,2] <- round(output$tab[,2], digits)
  output
}
