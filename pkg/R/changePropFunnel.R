#' Funnel plot of ratio and differences of proportions
#'
#' @description \code{changePropFunnel} can be used to compare units at two diferent periods. It plots a ratio (or difference) of proportions y versus a precision parameter rho. See details for more information.
#'
#' @param unit The unit names.
#' @param n1,n2 Total of admissions at 1st and 2nd periods, respectively.
#' @param o1,o2 Observed values at 1st and 2nd periods, respectively.
#' @param pi1,pi2 Mean values for in control instituitions at 1st and 2nd periods, respectively. Its assumed that O_i ~ Bin(pi_i,n_i)
#' @param method The indicator y type. It can be "diff" for a differences of proportions or "ratio" for a ratio of proportions. See details.
#' @param p Confidence level vector. It will return a confidence interval for all vector components. The default is 2 and 3 standard deviations (p = c(.95, 998))
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
#' @param xlab A title for the x axis. See \code{\link[graphics]{title}}
#' @param auto.ylab Logical; if \code{TRUE}, the plot is returned with default ylab.
#' @param ylab A title for the y axis. To change it one must set \code{auto.ylab = FALSE}. See \code{\link[graphics]{title}}
#' @param xlim,ylim Numeric vector giving the x and y coordinates ranges.
#' @param plot Logical; If \code{TRUE}, plots the correspondent graphic.
#' @param myunits A character vector with the unit names which one would like to benchmark among all units.
#' @param mypts.col The color passed to \code{\link[graphics]{points}} for the the units specified in \code{myunits}.
#' @param digits Integer indicating the number of decimals to be used in the output.
#' @param ... Further arguments passed to \code{\link[graphics]{plot}}.
#' @details
#' Suppose we have two measures for each institution: O1; N1 in a baseline period and O2; N2 in a subsequent period, and we wish to assess the change in the underlying proportion form pi1 to pi2. Two different measures might be of interest: the difference in proportions or the ratio of proportions. Normal approximations are used throughout, and for low (especially zero) counts \code{changePropFunnel} add 0.5 to all r’s and 1 to all n’s in order to stabilize the estimates.
#'
#' In case of \code{method = "diff"}, the indicator is Y = (O2/N2 - O1/N1) and theta = pi2-pi1. If \code{method = "ratio"}, the indicator is Y = (O2/N2)/(O1/N1) and theta = pi2/pi1. Also, it is convenient to work on a logarithmic scale, so that log(theta) is a target for log(Y). Theta is the target value which specifies the desired expectation for institutions considered "in control".
#'
#' For each cases, the precision parameter (plotted at x axis) can be interpreted as approximately the sample size per period.
#' @return A list with usefull information used in \code{\link{funnel}}.
#' @references
#' Spiegelhalter, David J. "Funnel plots for comparing institutional performance." Statistics in medicine 24.8 (2005): 1185-1202.
#' @import stats
#' @import graphics
#' @export


changePropFunnel <- function(unit, o1, o2, n1, n2, p = c(.95,.998), pi1 = sum(o1)/sum(n1), pi2 = sum(o2)/sum(n2), method = c("diff","ratio"), ..., col = c("skyblue4","skyblue2","snow4"), lwd = 2, lty = c(2,6,1), bty = "n", pch = 21, pt.col = "white", bg = "orange", pt.cex = 1.5, auto.legend = TRUE, text.cex = 0.7, text.pos = NULL, printUnits = FALSE, xlab = "Sample size per period", auto.ylab = TRUE, ylab = c("Proportions difference","Proportions ratio log"), ylim = c(max(lowerCI[[which(p == max(p))]]) - 6*theta, min(upperCI[[which(p == max(p))]]) + 6*theta), xlim = c(0,max(rho)), plot = FALSE, myunits = NULL, mypts.col = "darkblue", digits = 5){

  if(!is.factor(unit)){stop("Unit must be a factor.")}
  if(!is.numeric(n1)){stop("n1 must be numeric.")}
  if(!is.numeric(n2)){stop("n2 must be numeric.")}
  if (!is.numeric(o1)){stop("o1 must be numeric.")}
  if (!is.numeric(o2)){stop("o2 must be numeric.")}
  if (length(col) != length(p)+1){
    stop("col must have same length of p + 1 for the target line color in the last position")
  }
  if (!is.logical(auto.legend)){stop("auto.legend must be TRUE or FALSE.")}
  if (!is.logical(printUnits)){stop("printUnits must be TRUE or FALSE.")}
  if (!is.logical(auto.ylab)){stop("auto.ylab must be TRUE or FALSE.")}
  if (!is.logical(plot)){stop("plot must be TRUE or FALSE.")}
  if (method[1] != "diff" && method[1] != "ratio"){stop("method must be either 'diff' or 'ratio'.")}

  if (any(o1 == 0)){o1 <- o1 + .5} # To don't generate NaN values.
  if (any(o2 == 0)){o2 <- o2 + .5}
  if (any(n1 == 0)){n1 <- n1 + 1}
  if (any(n2 == 0)){n2 <- n2 + 1}

  upperCI <- list()
  lowerCI <- list()
  ylowCI <- list()
  yuppCI <- list()
  lowOUT <- list()
  uppOUT <- list()
  outofcontrol <- list()
  outcolname <- c()

  if (method[1] == "diff"){
    secondcolname <- "y"
    y <- (o2/n2) - (o1/n1)
    theta <- pi2 - pi1
    pim <- (pi2 + pi1) / 2 # mean proportion
    vary <- ((pim + (theta / 2)) * (1 - pim - (theta / 2))) / n2 + ((pim - (theta / 2)) * (1 - pim + (theta / 2))) / n1
    gdetheta <- ((pim + (theta / 2)) * (1 - pim - (theta / 2))) + ((pim - (theta / 2)) * (1 - pim + (theta / 2)))
    rho <- gdetheta / vary
    change.table <- data.frame(unit,y,o1,n1,o2,n2,rho)
    change.table <- change.table[order(change.table$rho),]
    unitnames <- data.frame(Unit = change.table$unit)
    expectedRange <- seq(0, max(change.table$rho)+5)

    for (i  in 1:(length(change.table$rho)-1)){ # do not allow repeted values in xCI
      if (ceiling(change.table$rho)[i] == ceiling(change.table$rho)[i+1]){
        change.table$rho[i+1] <- change.table$rho[i+1] + 1
      }
    }
    for (i in 1:length(p)){
      zp <- qnorm(1 - (1 - p[i]) / 2)
      upperCI[[i]] <- theta + zp * sqrt(gdetheta / expectedRange)
      lowerCI[[i]] <- theta - zp * sqrt(gdetheta / expectedRange)
      ylowCI[[i]] <- lowerCI[[i]][which(expectedRange %in% ceiling(change.table$rho) == TRUE)]
      yuppCI[[i]] <- upperCI[[i]][which(expectedRange %in% ceiling(change.table$rho) == TRUE)]
      lowOUT[[i]] <- ifelse(change.table$y < ylowCI[[i]],TRUE, FALSE)
      uppOUT[[i]] <- ifelse(change.table$y > yuppCI[[i]], TRUE, FALSE)
      outofcontrol[[i]] <- ifelse(lowOUT[[i]] == TRUE | uppOUT[[i]] == TRUE, "OUT","-")
      outcolname[i] <- paste0(p[i]*100,"%CI")
      change.table <- cbind(change.table, outofcontrol[[i]])
    }
    if (auto.ylab){ylab = ylab[1]}
  } else {
    # logarithmic scale!!
    secondcolname <- "log(y)"
    y <- (o2/n2) / (o1/n1)
    theta <- pi2 / pi1
    pig <- sqrt(pi2 * pi1)
    varlogy <- ((theta ^ (-1/2) - pig) / (n2*pig)) + (( theta ^ (1/2) - pig) / (n1*pig))
    gdetheta <- (theta ^ (-1/2) + theta ^ (1/2) - 2 * pig) / pig
    rho <- gdetheta / varlogy
    change.table <- data.frame(unit,"y" = log(y),o1,n1,o2,n2,rho)
    change.table <- change.table[order(change.table$rho),]
    unitnames <- data.frame(Unit = change.table$unit)
    expectedRange <- seq(0, max(change.table$rho)+5)

    for (i  in 1:(length(change.table$rho)-1)){ # do not allow more than 2 repeted values in xCI
      if (any(ceiling(change.table$rho)[(i+1):length(change.table$rho)] == ceiling(change.table$rho)[i])){
        change.table$rho[i] <- change.table$rho[i] + 1
      }
    }

    for (i  in 1:(length(change.table$rho)-1)){ # do not allow 2 repeted values in xCI
      if (ceiling(change.table$rho)[i] == ceiling(change.table$rho)[i+1]){
        change.table$rho[i+1] <- change.table$rho[i+1] + 1
      }
    }

    for (i in 1:length(p)){
      zp <- qnorm(1 - (1 - p[i]) / 2)
      upperCI[[i]] <- log(theta) + zp * sqrt(gdetheta / expectedRange)
      lowerCI[[i]] <- log(theta) - zp * sqrt(gdetheta / expectedRange)
      ylowCI[[i]] <- lowerCI[[i]][which(expectedRange %in% ceiling(change.table$rho) == TRUE)]
      yuppCI[[i]] <- upperCI[[i]][which(expectedRange %in% ceiling(change.table$rho) == TRUE)]
      lowOUT[[i]] <- ifelse(change.table$y < ylowCI[[i]],TRUE, FALSE)
      uppOUT[[i]]<- ifelse(change.table$y > yuppCI[[i]], TRUE, FALSE)
      outofcontrol[[i]] <- ifelse(lowOUT[[i]] == TRUE | uppOUT[[i]] == TRUE, "OUT","-")
      outcolname[i] <- paste0(p[i]*100,"%CI")
      change.table <- cbind(change.table, outofcontrol[[i]])
    }
    if (auto.ylab){ylab = ylab[2]}
  }

  x <- change.table$rho; y <- change.table$y; range <- expectedRange
  output <- list(x = x, y = y, theta = theta, range = range, tab = change.table, upperCI = upperCI, lowerCI = lowerCI, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, myunits = myunits, p = p, unitnames = unitnames, printUnits = printUnits)
  colnames(output$tab) <- c("Unit",secondcolname, "Obs1", "N1", "Obs2","N2","rho", outcolname)
  rownames(output$tab) <- seq(1,nrow(output$tab))
  output$tab[,2:7] <- round(output$tab[,2:7], digits)
  output
}
