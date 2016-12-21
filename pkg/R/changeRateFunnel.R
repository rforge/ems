#' Funnel Plot for Ratio of Rates
#'
#' @description \code{changeRateFunnel} can be used to compare units at two diferent periods. It plots a ratio of rates y versus a precision parameter rho. See details for more information.
#'
#' @param unit The unit names.
#' @param n1,n2 Total of admissions at 1st and 2nd periods, respectively.
#' @param o1,o2 Observed values at 1st and 2nd periods, respectively.
#' @param e1,e2 Expected values at 1st and 2nd periods, respectively.
#' @param lambda1,lambda2 Mean values for in control instituitions at 1st and 2nd periods, respectively. Its assumed that O_i ~ Poi(lambda_i)
#' @param y.type The indicator type. It can be SMR or SRU.
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
#' @param auto.xlab Logical; if \code{TRUE}, the plot is returned with default xlab.
#' @param xlab A title for the x axis. To change it one must set \code{auto.xlab = FALSE}. See \code{\link[graphics]{title}}
#' @param auto.ylab Logical; if \code{TRUE}, the plot is returned with default ylab.
#' @param ylab A title for the y axis. To change it one must set \code{auto.ylab = FALSE}. See \code{\link[graphics]{title}}
#' @param xlim,ylim Numeric vector giving the x and y coordinates ranges.
#' @param plot Logical; If \code{TRUE}, plots the correspondent graphic.
#' @param myunits A character vector with the unit names which one would like to benchmark among all units.
#' @param mypts.col The color passed to \code{\link[graphics]{points}} for the the units specified in \code{myunits}.
#' @param digits Integer indicating the number of decimals to be used in the output.
#' @param ... Further arguments passed to \code{\link[graphics]{plot}}.
#'
#' @details
#' Suppose we have two measures for each institution: O1; E1 in a baseline period and O2; E2 in a subsequent period, and we wish to assess the change in the underlying rate (SMR or SRU). We shall only consider the ratio of rates: exact methods based on a conditional argument are available if E1 = E2, and otherwise normal approximations are used, in which case for low (especially zero) counts one might add 0.5 to all Os and E’s.
#'
#' Y = (O1/E1)/(O2/E2) and the target theta =	lambda2/lambda1.
#'
#' Theta is the target value which specifies the desired expectation for institutions considered "in control".
#'
#' When E1 = E2, y is plotted versus the average observed count (rho).
#'
#' When E1 is different of E2, i.e., its used normal approximation, it is convenient to work on a logarithmic scale so that log(theta) is a target for log(Y). And y is plotted versus the expectation per period (rho)
#' @return A list with usefull information used in \code{\link{funnel}}.
#' @references
#' Spiegelhalter, David J. "Funnel plots for comparing institutional performance." Statistics in medicine 24.8 (2005): 1185-1202.
#' @seealso \code{\link{SMR}}, \code{\link{SRU}}
#' @import stats
#' @import graphics
#' @export


changeRateFunnel <- function(unit, n1, n2, o1, e1, o2, e2, lambda1 = sum(o1)/sum(n1), lambda2 = sum(o2)/sum(n2), y.type = c("SMR","SRU"), p = c(.95,.998), ..., col = c("skyblue4","skyblue2","snow4"), lwd = 2, lty = c(2,6,1), bty = "n", pch = 21, pt.col = "white", bg = "orange", pt.cex = 1.5, auto.legend = TRUE, text.cex = 0.7, text.pos = NULL, printUnits = FALSE, auto.xlab = TRUE, xlab = c("Average observed count","Expectation per period"), auto.ylab = TRUE, ylab = c(paste0(y.type[1],"'s Ratio"),paste0("Log(", y.type[1],"'s Ratio)")), ylim = c(max(lowerCI[[which(p == max(p))]]) - 1.5*theta, min(upperCI[[which(p == max(p))]]) + 1.5*theta), xlim = c(0,max(rho)), plot = FALSE, myunits = NULL, mypts.col = "darkblue", digits = 5){

  if(!is.factor(unit)){stop("Unit must be a factor.")}
  if(!is.numeric(n1)){stop("n1 must be numeric.")}
  if(!is.numeric(n2)){stop("n2 must be numeric.")}
  if (!is.numeric(o1)){stop("o1 must be numeric.")}
  if (!is.numeric(o2)){stop("o2 must be numeric.")}
  if (!is.numeric(e1)){stop("e1 must be numeric.")}
  if (!is.numeric(e2)){stop("e2 must be numeric.")}
  if (y.type[1] != "SMR" && y.type[1] != "SRU"){
    stop("y.type must be either 'SMR' or 'SRU'.")
    }
  if (!is.vector(p)){stop("p must be a vector.")}
  if (length(col) != length(p)+1){
    stop("col must have same length of p + 1 for the target line color in the last position")
  }
  if (!is.logical(auto.legend)){stop("auto.legend must be TRUE or FALSE.")}
  if (!is.logical(printUnits)){stop("printUnits must be TRUE or FALSE.")}
  if (!is.logical(auto.xlab)){stop("auto.xlab must be TRUE or FALSE.")}
  if (!is.logical(auto.ylab)){stop("auto.ylab must be TRUE or FALSE.")}
  if (!is.logical(plot)){stop("plot must be TRUE or FALSE.")}

  if (any(o1 == 0)){o1 <- o1 + .5} #To don't generate NaN values.
  if (any(o2 == 0)){o2 <- o2 + .5}
  if (any(e1 == 0)){e1 <- e1 + .5}
  if (any(e2 == 0)){e2 <- e2 + .5}

  y <- (o2/e2)/(o1/e1)
  theta <- lambda2/lambda1
  upperCI <- list()
  lowerCI <- list()
  ylowCI <- list()
  yuppCI <- list()
  lowOUT <- list()
  uppOUT <- list()
  outofcontrol <- list()
  outcolname <- c()

  if (all.equal(e1, e2) == TRUE){
    # then y = o2/o1
    # exact methods
    secondcolname <- "y"
    rho <- (o1 + o2)/2  #precision parameter: average observed count
    change.table <- data.frame(unit,y,o1,e1,n1,o2,e2,n2,rho)
    change.table <- change.table[order(change.table$rho),]
    unitnames <- data.frame(Unit = change.table$unit)
    expectedRange <- seq(1, max(change.table$rho)+5)
    lambda <- theta*expectedRange

    for (i  in 1:(length(change.table$rho)-1)){ # do not allow repeted values in xCI
      if (ceiling(change.table$rho)[i] == ceiling(change.table$rho)[i+1]){
        change.table$rho[i+1] <- change.table$rho[i+1] + 1
      }
    }
    for (i in 1:length(p)){
      rp <- qpois(p[i], lambda)
      alpha <- (ppois(rp, lambda) - p[i]) / (ppois(rp, lambda) - ppois(rp - 1, lambda))
      upperCI[[i]] <- theta + (rp - alpha) / expectedRange
      lowerCI[[i]] <- theta - (rp - alpha) / expectedRange
      ylowCI[[i]] <- lowerCI[[i]][which(expectedRange %in% ceiling(change.table$rho) == TRUE)]
      yuppCI[[i]] <- upperCI[[i]][which(expectedRange %in% ceiling(change.table$rho) == TRUE)]
      lowOUT[[i]] <- ifelse(change.table$y < ylowCI[[i]],TRUE,FALSE)
      uppOUT[[i]]<- ifelse(change.table$y > yuppCI[[i]], TRUE, FALSE)
      outofcontrol[[i]] <- ifelse(lowOUT[[i]] == TRUE | uppOUT[[i]] == TRUE, "OUT","-")
      outcolname[i] <- paste0(p[i]*100,"%CI")
      change.table <- cbind(change.table, outofcontrol[[i]])
    }
    if (auto.xlab){xlab = xlab[1]}
    if (auto.ylab){ylab = ylab[1]}
  } else {
    # normal approximation
    # logarithmic scale!!
      secondcolname <- "log(y)"
      lambdaghat <- (o1 + o2) / (theta ^ (1/2) * e2 + theta ^ (-1/2) * e1)
      lambdagbarra <- (sum(o1) + sum(o2)) / (sum(e1) + sum(e2)) #assuming e1 = e2 = e, then varlogy|lambdagbarra = gdethetha / e
      varlogy <- ((theta ^ (-1/2)) / (e2* lambdaghat)) + ((theta ^ (1/2)) / (e1* lambdaghat))
      gdetheta <- (theta ^ (-1/2) + theta ^ (1/2)) / lambdagbarra
      rho <- gdetheta / varlogy # precision parameter
      change.table <- data.frame(unit,"y" = log(y),o1,e1,n1,o2,e2,n2,rho)
      change.table <- change.table[order(change.table$rho),]
      unitnames <- data.frame(Unit = change.table$unit)
      expectedRange <- seq(1, max(change.table$rho)+5)
    for (i  in 1:(length(change.table$rho)-1)){ # do not allow repeted values in xCI
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
    if (auto.xlab){xlab = xlab[2]}
    if (auto.ylab){ylab = ylab[2]}
  }

  x <- change.table$rho; y <- change.table$y; range <- expectedRange
  output <- list(x = x, y = y, theta = theta, range = range, tab = change.table, upperCI = upperCI, lowerCI = lowerCI, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, myunits = myunits, p = p, unitnames = unitnames, printUnits = printUnits)
  colnames(output$tab) <- c("Unit",secondcolname, "Obs1", "Exp1","N1", "Obs2","Exp2","N2","rho", outcolname)
  rownames(output$tab) <- seq(1,nrow(output$tab))
  output$tab[,2:9] <- round(output$tab[,2:9], digits)
  output
}
