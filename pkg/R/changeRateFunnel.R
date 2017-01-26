
#' @import stats
#' @import graphics


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
    rho <- (o1 + o2)/2  # precision parameter: average observed count
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
      lowOUT[[i]] <- ifelse(change.table$y < ylowCI[[i]], TRUE, FALSE)
      uppOUT[[i]]<- ifelse(change.table$y > yuppCI[[i]], TRUE, FALSE)
      outofcontrol[[i]] <- ifelse(lowOUT[[i]] == TRUE | uppOUT[[i]] == TRUE, "OUT","-")
      outcolname[i] <- paste0(p[i]*100, "%CI")
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

      theta <- log(theta)
  }

  x <- change.table$rho; y <- change.table$y; range <- expectedRange
  output <- list(x = x, y = y, theta = theta, range = range, tab = change.table, upperCI = upperCI, lowerCI = lowerCI, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, myunits = myunits, p = p, unitnames = unitnames, printUnits = printUnits)
  colnames(output$tab) <- c("Unit",secondcolname, "Obs1", "Exp1","N1", "Obs2","Exp2","N2","rho", outcolname)
  rownames(output$tab) <- seq(1,nrow(output$tab))
  output$tab[,2:9] <- round(output$tab[,2:9], digits)
  output
}
