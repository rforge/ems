#' Funnel Plot
#'
#' @name funnel
#'
#' @description Produces a variety of data funnel plots. Funnel plots are usefull for identifing units which are "in control" or not.
#'
#' @param unit The unit names.
#' @param y Standardized rate vector, as SMR or SRU, accordind to \code{y.type}. It's also called "indicator".
#' @param n Volume of cases, admissions, for each unit.
#' @param n1,n2 Total of admissions at 1st and 2nd periods, respectively.
#' @param o Observed death vector. Accepted values are 0 (absence) or 1 (presence).
#' @param o1,o2 Observed values at 1st and 2nd periods, respectively.
#' @param e Expected death vector. Used when \code{direct = FALSE}.
#' @param e1,e2 Expected values at 1st and 2nd periods, respectively.
#' @param lambda1,lambda2 Mean values for in control instituitions at 1st and 2nd periods, respectively. Its assumed that O_i ~ Poi(lambda_i). Used when \code{option = ratioRates}
#' @param pi1,pi2 Mean values for in control instituitions at 1st and 2nd periods, respectively. Its assumed that O_i ~ Bin(pi_i,n_i). Used when \code{option = diffProp or ratioProp}
#' @param y.type The indicator type. It can be SMR or SRU. (used for \code{option = rate or ratioRates})
#' @param p Confidence level vector. It will return a confidence interval for all vector components. The default is 2 and 3 standard deviations (p = c(.95, 998)).
#' @param theta Target value which specifies the desired expectation for institutions considered "in control". Used when \code{option = prop or rate}
#' @param direct Logical; Used when \code{option = rate}. If \code{TRUE}, we assume the rates are reported as a rate per (say) 1000 individuals, and that the rate has been transformed to a proportion y between 0 and 1. The measure of the associated error may be reported in the size of population \code{n} (CI is made a binomial approximation). If \code{FALSE}, it may be reported in the population expected death \code{e} (CI is made a poisson approximation). See details.
#' @param method There are two kind of approximations, as discussed in \code{direct} parameter. Inside them, there are two options:  to make the CI from the exact distribuition (binomial or poisson) or from de normal distribution. So, here one could choose between \code{normal} or \code{exact} (default). See details.
#' @param myunits A character vector with the unit names which one would like to benchmark among all units.
#' @param overdispersion Logical; If \code{TRUE}, introduces an multiplicative over-dispersion factor phi that will inflate the normal CI null variance. See details.
#' @param option The type of funnel plot one wants to produce. It can be equal \code{rate, ratioRates, prop, diffProp or ratioProp}. See details to understand how to use each of them.
#' @param printUnits Logical; If \code{TRUE}, the units are identified in the plot and printed in de console. The numbers in the plot correspond to the data.frame printed in the console.
#' @param plot Logical; If \code{TRUE}, the correspondent graphic is plotted.
#' @param x An object of class 'funnel'.
#' @param col Especification vector for the CI lines colors. Must have same length of \code{p} + 1 with the target line in the last position.
#' @param lwd The lines width, a positive number. It's the same for all lines in the plot. See \code{\link[graphics]{par}}.
#' @param lty The CI lines types. See \code{\link[graphics]{par}}.
#' @param bty A character string which determined the type of \code{\link[graphics]{box}} which is drawn about plots. See \code{\link[graphics]{par}}.
#' @param pch Either an integer specifying a symbol or a single character to be used as the default in plotting points. See \code{\link[graphics]{points}} for possible values and their interpretation. Note that only integers and single-character strings can be set as a graphics parameter (and not NA nor NULL).
#' @param pt.col Especification vector for the points colors.
#' @param bg The color to be used for the background of the points for \code{pch = 21}. See \code{\link[graphics]{par}}.
#' @param pt.cex A numerical value giving the amount by which plotting points should be magnified relative to the default.  See \code{\link[graphics]{par}}.
#' @param auto.legend Logical; If \code{TRUE}, prints a legend with default arguments.
#' @param text.cex Like \code{pt.cex}, but for the texts numbers correspondents to the units.
#' @param text.pos a position specifier for numbers that correspond to the units in the plot. Values of 1, 2, 3 and 4, respectively indicate positions below, to the left of, above and to the right of the points.
#' @param mypts.col The color passed to \code{\link[graphics]{points}} for the the units specified in \code{myunits}.
#' @param xlab,ylab A title for the x and y axis. See \code{\link[graphics]{title}}
#' @param digits Integer indicating the number of decimals to be used in the output.
#' @param ... Further arguments passed to \code{\link[graphics]{plot}}.
#'
#' @details
#' \itemize{
#' \item If \code{option = rate}, \code{funnel} plots a standardized rate y versus its expected death or volume value for several units.
#'
#' To choose the \code{direct} argument, one should pay attention if one wants to use a Direct Standardized Rate or a Indirect Standardized Rate. If direct, we assume the rate is reported as a rate per (say) 1000 individuals, then it is treated as a proportion. If indirect, it is a cross-sectional data that leads to a standardized event ratio.
#'
#' In many circumstances we can assume an exact or approximate normal distribution for the data. Using the \code{type} argument, one could choose between \code{exact} or  \code{normal}. For direct standardized rates, the exact distribuition is binomial and for indirect standardized rates, the exact distribuition is poisson. Assume rho is the precision parameter (volume, for direct rates; expected value, for indirect rates). For rho > 100 the normal and exact curves almost coincide. So, one could perfectly use  normal approximation if ones data parameter precision is greater than 100, in general.
#'
#' The console warns if there are units with volume/expected value less than 100.
#'
#' \item If \code{option = ratioRate}, \code{funnel} can be used to compare units at two diferent periods. It plots a ratio of rates y versus a precision parameter rho.
#'
#' Suppose we have two measures for each institution: O1; E1 in a baseline period and O2; E2 in a subsequent period, and we wish to assess the change in the underlying rate (SMR or SRU). We shall only consider the ratio of rates: exact methods based on a conditional argument are available if E1 = E2, and otherwise normal approximations are used, in which case for low (especially zero) counts one might add 0.5 to all Os and E’s.
#'
#' Y = (O1/E1)/(O2/E2) and the target theta =	lambda2/lambda1.
#'
#' Theta is the target value which specifies the desired expectation for institutions considered "in control".
#'
#' When E1 = E2, y is plotted versus the average observed count (rho).
#'
#' When E1 is different of E2, i.e., its used normal approximation, it is convenient to work on a logarithmic scale so that log(theta) is a target for log(Y). And y is plotted versus the expectation per period (rho)
#'
#' \item If \code{option = prop}, \code{funnel} plots a proportion y versus its volume.
#' It is used for cross-sectional data. Suppose in each institution that o events are observed out of a sample size of n:
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
#'
#' \item If \code{option = ratioProp or diffProp}, \code{funnel} can be used to compare units at two diferent periods. It plots a ratio (or difference) of proportions y versus a precision parameter rho.
#'
#'  Suppose we have two measures for each institution: O1; N1 in a baseline period and O2; N2 in a subsequent period, and we wish to assess the change in the underlying proportion form pi1 to pi2. Two different measures might be of interest: the difference in proportions or the ratio of proportions. Normal approximations are used throughout, and for low (especially zero) counts \code{changePropFunnel} add 0.5 to all r’s and 1 to all n’s in order to stabilize the estimates.
#'
#' In case of \code{method = "diff"}, the indicator is Y = (O2/N2 - O1/N1) and theta = pi2-pi1. If \code{method = "ratio"}, the indicator is Y = (O2/N2)/(O1/N1) and theta = pi2/pi1. Also, it is convenient to work on a logarithmic scale, so that log(theta) is a target for log(Y). Theta is the target value which specifies the desired expectation for institutions considered "in control".
#'
#' For each cases, the precision parameter (plotted at x axis) can be interpreted as approximately the sample size per period.
#'
#'}
#'
#'If \code{overdispersion = TRUE}, the normal CI ins inflated by a overdispersion parameter phi. It is kind of arbitrary and usefull in high-volume outcome measures, when the large majority of institutions lie outside the funnel, casting doubt on the appropriateness of the limits.
#'
#' phi = (1/total) * sum((y - theta) ^ 2 * e) / theta
#'
#' \eqn{var(y|theta,n) = (phi * g(theta)) / e}
#'
#' @return A table with unit names, y, observed (Obs), expected (Exp) and admissions (N) for each unit, and final columns show which units are out of control.
#' @references
#' Spiegelhalter, David J. "Funnel plots for comparing institutional performance." Statistics in medicine 24.8 (2005): 1185-1202.
#' @examples
#' # Loading data
#' data(icu)
#'
#' # Getting the cross-sectional arguments to use in funnel
#' x <- SMR.table(data = icu, group.var = "Unit",
#' obs.var = "UnitDischargeName", pred.var = "Saps3DeathProbabilityStandardEquation")
#'
#' # to analyse proportions
#' plot(funnel(unit = x$Levels[-1], o = x[-1,]$Observed, theta = x$Observed[1] / x$N[1],
#'  n = x[-1,]$N, method = "exact", myunits = c("A"), option = "prop", plot = FALSE), main = "Cross-sectional proportions")
#'
#' # to analyse rates (SMR)
#' plot(funnel(unit = x$Levels[-1], y = x[-1,]$SMR, method = "exact", direct = TRUE,
#' theta = x$SMR[1], e = x[-1,]$Expected, n = x[-1,]$N, o = x[-1,]$Observed,
#' option = "rate", myunits = NULL, plot = FALSE), main = "Cross-sectional rate (SMR)")
#'
#' # creating a variable containing month information about each admission
#'  icu$month <- as.numeric(format(as.Date(icu$UnitAdmissionDate),"%m"))
#'
#' # First quarter
#' dt1 <- icu[which(icu$month %in% c(1,2,3)),]
#'
#' # Second quarter
#' dt2 <- icu[which(icu$month %in% c(4,5,6)),]
#'
#' # Getting the two period arguments to use in funnel
#' z <- SMR.table(data = dt1, group.var = "Unit", obs.var = "UnitDischargeName",
#'  pred.var = "Saps3DeathProbabilityStandardEquation")
#' w <- SMR.table(data = dt2, group.var = "Unit", obs.var = "UnitDischargeName",
#'  pred.var = "Saps3DeathProbabilityStandardEquation")
#'
#'  # to analyse periods using ratio rates with e1 = e1
#'  plot(funnel(unit <- z$Levels[-1], n1 = z$N[-1], o1 = z$Observed[-1], e1 = z$Expected[-1],
#'  n2 = w$N[-1], o2 = w$Observed[-1], e2 = z$Expected[-1],
#'  myunits = c("A","B"), option = "ratioRates", plot = FALSE), main = "Ratio of SMRs of periods with same expectation of death")
#'
#'  # to analyse periods using ratio rates with e1 =! e1
#'  plot(funnel(unit <- z$Levels[-1], n1 = z$N[-1], o1 = z$Observed[-1],
#'  e1 = z$Expected[-1], n2 = w$N[-1], o2 = w$Observed[-1], e2 = w$Expected[-1], option = "ratioRates", plot = FALSE), main = "Ratio of SMRs of periods with different expectation of death")
#'
#'  # to analyse periods by difference in proportions
#'  plot(funnel(unit <- z$Levels[-1], n1 = z$N[-1], o1 = z$Observed[-1],
#'  n2 = w$N[-1], o2 = w$Observed[-1], method = "diff", option = "diffProp", plot = FALSE), main = "Difference in proportions of death for two periods")
#'
#'  # to analyse periods by ratio of proportions
#'  plot(funnel(unit <- z$Levels[-1], n1 = z$N[-1], o1 = z$Observed[-1],
#'  n2 = w$N[-1], o2 = w$Observed[-1], method = "diff", option = "ratioProp", plot = FALSE), main = "Ratio of proportions of death for two periods")
#'
#'  rm(icu, x, z, w, dt1, dt2, unit )
#'
#' @import stats
#' @import graphics
#' @export


funnel <- function(unit, y, n, n1, n2, o, o1, o2, e, e1, e2, lambda1 = sum(o1)/sum(n1), lambda2 = sum(o2)/sum(n2), pi1 = sum(o1)/sum(n1), pi2 = sum(o2)/sum(n2), y.type = c("SMR","SRU"), p = c(.95,.998), theta, method = c("exact","normal"), direct = FALSE, myunits = NULL, overdispersion = FALSE, option = c("rate", "ratioRates", "prop", "diffProp", "ratioProp"), printUnits = TRUE, plot = TRUE, digits = 5){

  if (option[1] != "rate" && option[1] != "ratioRates" && option[1] != "prop" && option[1] != "diffProp" && option[1] != "ratioProp"){stop("option must be either 'rate', 'ratioRates', 'prop', 'diffprop' or 'ratioProp'.")}

  if (option[1] == "rate"){
    output <- rateFunnel(unit, y, n, o, e, y.type, p, theta, method, direct, myunits = myunits, printUnits = printUnits, digits = digits)
  }
  if (option[1] == "ratioRates"){
    output <- changeRateFunnel(unit, n1, n2, o1, e1, o2, e2, lambda1, lambda2, y.type, p, myunits = myunits, printUnits = printUnits, digits = digits)
  }
  if (option[1] == "prop"){
    output <- propFunnel(unit, o, n, theta, p, method, myunits = myunits, printUnits = printUnits, digits = digits)
  }
  if (option[1] == "diffProp"){
    output <- changePropFunnel(unit, o1, o2, n1, n2, p, pi1, pi2, method = "diff", myunits = myunits, printUnits = printUnits, digits = digits)
  }
  if (option[1] == "ratioProp"){
    output <- changePropFunnel(unit, o1, o2, n1, n2, p, pi1, pi2, method = "ratio", myunits = myunits, printUnits = printUnits, digits = digits)
  }

  class(output) <- "funnel"
  if (plot) {plot(output)}
  if (printUnits) {output}

}

#' @rdname funnel
#' @export
print.funnel <- function(x,...){
  print(x$tab)
}

#' @rdname funnel
#' @export
plot.funnel <- function(x, ...,col = c("skyblue4","skyblue2","snow4"), lwd = 2, lty = c(2,6,1), bty = "n", pch = 21, pt.col = "white", bg = "orange", pt.cex = 1.5, auto.legend = TRUE, text.cex = 0.7, text.pos = NULL, mypts.col = "darkblue", printUnits = x$printUnits, xlab = x$xlab, ylab = x$ylab){

    plot(x$x, x$y, ..., type = "n", xlim = x$xlim, ylim = x$ylim, bty = bty, xlab = xlab, ylab = ylab)
    abline(h = x$theta, col = col[length(x$p)+1], lwd = lwd)
    for (i in 1:length(x$p)){
      lines(x$range, x$upperCI[[i]], col = col[i], lwd = lwd, lty = lty[i])
      lines(x$range, x$lowerCI[[i]], col = col[i], lwd = lwd, lty = lty[i])
    }
    points(x$x, x$y, pch = pch, col = pt.col, bg = bg, cex = pt.cex)

    if (length(x$myunits) > 0){
      points(x$x[which(x$unitnames$Unit %in% x$myunits)], x$y[which(x$unitnames$Unit %in% x$myunits)], pch = pch, col = pt.col, bg = mypts.col, cex = pt.cex)
    }
    if (printUnits) {
      if (length(x$myunits) > 0 ){

        text(x$x[-which(x$unitnames$Unit %in% x$myunits)], x$y[-which(x$unitnames$Unit %in% x$myunits)], labels = rownames(x$unitnames)[-which(x$unitnames$Unit %in% x$myunits)], cex = text.cex, pos = text.pos)

        text(x$x[which(x$unitnames$Unit %in% x$myunits)], x$y[which(x$unitnames$Unit %in% x$myunits)], labels = rownames(x$unitnames)[which(x$unitnames$Unit %in% x$myunits)], cex = text.cex, pos = length(text.pos) + 1)
      } else {
        text(x$x, x$y, labels = rownames(x$unitnames), cex = text.cex, pos = text.pos)
      }
    }
    if (auto.legend){
      legend.arg <- c()
      for (i in 1:length(x$p)){
        legend.arg <- append(legend.arg, paste0(x$p[i]*100,"% limits"))
      }
      legend.arg <- append(legend.arg, paste0("Theta = ",format(round(x$theta,2), nsmall = 2)))
      legend(x = "topright", legend = as.expression(legend.arg), lwd = lwd, lty = lty, bty = "n", col = col[0:length(x$p)+1])
    }
    on.exit(par(mfrow = c(1,1)))
}
