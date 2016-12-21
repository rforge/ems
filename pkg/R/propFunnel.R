#' Funnel Plot for Proportions
#'
#' @description \code{propFunnel} Plots a proportion y versus its volume.
#'
#' @param unit The unit names.
#' @param n Total of admissions at 1st and 2nd periods, respectively.
#' @param o Observed values at 1st and 2nd periods, respectively.
#' @param theta Target value which specifies the desired expectation for institutions considered "in control". It must be a proportion. (0 < theta < 1)
#' @param p Confidence level vector. It will return a confidence interval for all vector components. The default is 2 and 3 standard deviations (p = c(.95, 998))
#' @param method There are two kind of approximations, binomial (exact) or normal. So, here one could choose between \code{normal} or \code{exact} (default).
#' @param col Especification vector for the CI lines colors. Must have same length of \code{p} + 1 for the target line in the last position.
#' @param lwd The lines width, a positive number. It's the same for ao lines in the plot. See \code{\link[graphics]{par}}
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
#' @param xlab,ylab A title for the x and y axis. See \code{\link[graphics]{title}}
#' @param xlim,ylim Numeric vector giving the x and y coordinates ranges.
#' @param plot Logical; If \code{TRUE}, plots the correspondent graphic.
#' @param myunits A character vector with the unit names which one would like to benchmark among all units.
#' @param mypts.col The color passed to \code{\link[graphics]{points}} for the the units specified in \code{myunits}.
#' @param overdispersion Logical; If \code{TRUE}, introduces an multiplicative over-dispersion factor phi that will inflate the normal CI null variance. See details.
#' @param digits Integer indicating the number of decimals to be used in the output.
#' @param ... Further arguments passed to \code{\link[graphics]{plot}}.
#'
#' @details
#' \code{propFunnel} is used for cross-sectional data. Suppose in each institution that o events are observed out of a sample size of n:
#'
#' The indicator is the observed proportion y = o/n
#'
#' Assume \code{n} is the precision parameter (volume). For \code{n} > 100 the normal and exact curves almost coincide. So, one could perfectly use  normal approximation if ones data parameter precision is greater than 100, in general.
#'
#' If \code{overdispersion = TRUE}, the normal CI ins inflated by a overdispersion parameter phi. It is kind of arbitrary and usefull in high-volume outcome measures, when the large majority of institutions lie outside the funnel, casting doubt on the appropriateness of the limits.
#'
#' phi = (1/total) * sum((y - theta) ^ 2 * n)/g(theta)
#'
#' var(y|theta,n) = (phi * g(theta))/n
#' @return A list with usefull information used in \code{\link{funnel}}.
#' @references
#' Spiegelhalter, David J. "Funnel plots for comparing institutional performance." Statistics in medicine 24.8 (2005): 1185-1202.
#' @import stats
#' @import graphics
#' @export


propFunnel <- function(unit, o, n, theta, p = c(.95,.998), method = c("exact","normal"), ..., col = c("skyblue4","skyblue2","snow4"), lwd = 2, lty = c(2,6,1), bty = "n", pch = 21, pt.col = "white", bg = "orange", pt.cex = 1.5, auto.legend = TRUE, text.cex = 0.7, text.pos = NULL, printUnits = FALSE, ylab = "%", xlab = "Volume", ylim = c(0, min(upperCI[[which(p == max(p))]]) + 2.5*theta), xlim = c(0, max(n)), plot  = FALSE, myunits = NULL, mypts.col = "darkblue", overdispersion = FALSE, digits = 5){

  if(!is.factor(unit)){stop("Unit must be a factor.")}
  if(!is.numeric(n)){stop("n must be numeric.")}
  if (!is.numeric(o)){stop("o must be numeric.")}
  if (!is.numeric(theta)){stop("theta must be numeric.")}
  if (theta < 0 | theta > 1){stop("theta must be between 0 and 1.")}
  if (!is.vector(p)){stop("p must be a vector.")}
  if (length(col) != length(p)+1){
    stop("col must have same length of p + 1 for the target line color in the last position")
  }
  if (method[1] != "normal" && method[1] != "exact"){stop("method must be either 'normal' or 'exact'.")}
  if (!is.logical(auto.legend)){stop("auto.legend must be TRUE or FALSE.")}
  if (!is.logical(printUnits)){stop("printUnits must be TRUE or FALSE.")}
  if (!is.logical(plot)){stop("plot must be TRUE or FALSE.")}
  if (!is.logical(overdispersion)){stop("overdispersion must be TRUE or FALSE.")}

  y <- (o / n) * 100
  theta <- theta * 100
  gdetheta <- theta * (100 - theta)
  # n = n precision parameter

  upperCI <- list()
  lowerCI <- list()
  ylowCI <- list()
  yuppCI <- list()
  lowOUT <- list()
  uppOUT <- list()
  outofcontrol <- list()
  outcolname <- c()

  prop.table <- data.frame(unit, y, o, n)
  prop.table <- prop.table[order(prop.table$n),]
  unitnames <- data.frame(Unit = prop.table$unit)
  admissionsRange <- seq(1,max(n))
  observedRange <- seq(1, max(o), length.out = length(admissionsRange))
  prob <- observedRange / admissionsRange

  # using exact binomial approximation
  if (method[1] == "exact"){
    for (i in 1:length(p)){
      rp <- qbinom(p[i], size = admissionsRange, prob)
      alpha <- (pbinom(rp, size = admissionsRange, prob) - p[i]) / ((pbinom(rp, size = admissionsRange, prob)) - pbinom(rp - 1, size = admissionsRange, prob))
      upperCI[[i]] <- theta + ((rp - alpha) / admissionsRange) * 100
      lowerCI[[i]] <- theta - ((rp - alpha) / admissionsRange) * 100
      ylowCI[[i]] <- lowerCI[[i]][which(admissionsRange %in% prop.table$n == TRUE)]
      yuppCI[[i]] <- upperCI[[i]][which(admissionsRange %in% prop.table$n == TRUE)]
      lowOUT[[i]] <- ifelse(prop.table$y < ylowCI[[i]],TRUE,FALSE)
      uppOUT[[i]]<- ifelse(prop.table$y > yuppCI[[i]], TRUE, FALSE)
      outofcontrol[[i]] <- ifelse(lowOUT[[i]] == TRUE | uppOUT[[i]] == TRUE, "OUT","-")
      outcolname[i] <- paste0(p[i]*100,"%CI")
      prop.table <- cbind(prop.table, outofcontrol[[i]])
    }
  } else { # using normal approximation
     if (overdispersion){
       phi <- (1/nrow(prop.table)) * sum(((y - theta) ^ 2 * n)/(gdetheta))
       for (i in 1:length(p)){
        zp <- qnorm(1 - (1 - p[i]) / 2)
        upperCI[[i]] <- theta + zp * sqrt(gdetheta * phi / admissionsRange)
        lowerCI[[i]] <- theta - zp * sqrt(gdetheta * phi / admissionsRange)
       }
     } else {
    for (i in 1:length(p)){
      zp <- qnorm(1 - (1 - p[i]) / 2)
      upperCI[[i]] <- theta + zp * sqrt(gdetheta / admissionsRange)
      lowerCI[[i]] <- theta - zp * sqrt(gdetheta / admissionsRange)
      ylowCI[[i]] <- lowerCI[[i]][which(admissionsRange %in% prop.table$n == TRUE)]
      yuppCI[[i]] <- upperCI[[i]][which(admissionsRange %in% prop.table$n == TRUE)]
      lowOUT[[i]] <- ifelse(prop.table$y < ylowCI[[i]],TRUE,FALSE)
      uppOUT[[i]]<- ifelse(prop.table$y > yuppCI[[i]], TRUE, FALSE)
      outofcontrol[[i]] <- ifelse(lowOUT[[i]] == TRUE | uppOUT[[i]] == TRUE, "OUT","-")
      outcolname[i] <- paste0(p[i]*100,"%CI")
      prop.table <- cbind(prop.table, outofcontrol[[i]])
      }
     }
  }

  x <- n; y <- y; range <- admissionsRange
  output <- list(x = x, y = y, theta = theta, range = range, tab = prop.table, upperCI = upperCI, lowerCI = lowerCI, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, myunits = myunits, p = p, unitnames = unitnames, printUnits = printUnits)
  colnames(output$tab) <- c("Unit","y", "Observed","Admissions", outcolname)
  rownames(output$tab) <- seq(1,nrow(output$tab))
  output$tab$y <- round(output$tab$y, digits)
  output
}
