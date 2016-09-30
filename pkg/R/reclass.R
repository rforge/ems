#' Comparisson of the Standardized Resource Use (SRU)
#'
#' @name reclass
#'
#' @description To compare ICUs  with diferents severities classes or throught two diferents times about your efficiency classification.
#'
#' \code{plot.reclass} Plots a SMR vs. SRU scatter plot with the ICUs which had your classification changed.
#'
#' \code{print.reclass} Prints a table with information about which ICUs changed from a classification to another.
#'
#' @param x,y Objects of class 'SRU'. Be x from the 1st stage and y from the 2nd. For \code{print.reclass} or \code{plot.reclass}, x is an object of class 'reclass'.
#' @param same logical; If TRUE, compare the same units, with the same severity classes through two diferents times (default). If FLASE compare the same units, with the different severity classes.
#' @param plot logical; If TRUE, plots a SMR vs. SRU scatter plot with the ICUs which had your classification changed.
#' @param digits Integer indicating the number of decimal places to be used in the output.
#' @param compare The way one prefer to benchmark the ICUs: by SRU (default), SMR or both of them.
#' @param decreasing logical; Should the sort order of ICU's rank be increasing or decreasing?
#' @return A table with information about which ICUs changed from a classification to another, their SMR, SRU and rank among other ICUs.
#'
#' @seealso \code{\link{SRU}}
#'
#' @author Lunna Borges and Pedro Brasil
#'
#' @examples
#' data(icu)
#' #1st quarter
#' x <- icu[which(format(as.Date(icu$UnitAdmissionDate),"%m") %in% c("01","02","03")),]
#'
#' #2nd quarter
#' y <- icu[which(format(as.Date(icu$UnitAdmissionDate),"%m") %in% c("04","05","06")),]
#'
#' FirstQ <- SRU(prob = x$Saps3DeathProbabilityStandardEquation, death = x$UnitDischargeName, unit = x$Unit, los = x$los, score = x$Saps3Points, originals = T, type = 1, plot = F)
#' FirstQ
#'
#' SecondQ <- SRU(prob = y$Saps3DeathProbabilityStandardEquation, death = y$UnitDischargeName, unit = y$Unit, los = y$los, score = y$Saps3Points, originals = T, type = 1, plot = F)
#' SecondQ
#'
#' reclass(x = FirstQ, y = SecondQ)
#'
#' plot(reclass(x = FirstQ, y = SecondQ))
#'
#' rm(icu, x, y, FirstQ, SecondQ)
#'
#' @export
reclass <- function(x, y, same = TRUE, plot = FALSE, digits = 2, compare = c("SRU","SMR","BOTH"),decreasing = FALSE){
  if(class(x) != "SRU" || class(y)!= "SRU"){
    stop("'x','y' must be objects of class 'SRU'.")
  }
  x <- x$rates; y <- y$rates
  if(!is.data.frame(x)){
    stop("'x$rates' must be a data frame.")
  }
  if(!is.data.frame(y)){
    stop("'y$rates' must be a data frame.")
  }
  if(compare[1] != "SMR" && compare[1] != "SRU" && compare[1] != "BOTH"){
    stop("Compare method must be either 'SMR' or 'SRU' or 'BOTH'.")
  }
  if(compare[1] == "BOTH"){
    warning(paste0("As compare = BOTH, we rank the ICUs by their SRU."))
  }
  if(!is.logical(same)){
    stop("Same must be either 'TRUE' or 'FALSE'.")
  }
  if(same){
    a <- x[which(x$unit %in% y$unit),]
    b <- y[which(y$unit %in% a$unit),]
    warning(paste0(c("Some units were excluded because their absence in the 1st stage x.")))
    x <- droplevels(a); y <- droplevels(b)
  } else {
    if(nrow(x) != nrow(y)){
      stop("To compare two diferents models x and y must have the same ICUs.")
    }
    if (compare[1] == "SMR"){
      warning(paste0("SMR is the same in both models."))
    }
  }
  if(ncol(x) != 4 | ncol(y) != 4 ){
    stop("x$rates and y$rates must have 4 columns each.")
  }
  if (!all.equal(x[,3], y[,3])){
    stop("x e y must have informations about the same ICUs.")
  }
  if(isTRUE(all.equal(x[,4], y[,4]))){
    stop("There isn't any change.")
  }

  dt <- data.frame("Unit" = x$unit, "Now" = x$group, "After" = y$group)
  dt$change <- ifelse(dt$Now != dt$After, "CHANGE", "SAME")

  nchanges <- length(which(dt$change == "CHANGE"))
  change_from_to <- data.frame("Unit" = dt$Unit[which(dt$change == "CHANGE")], "From" = dt$Now[which(dt$change == "CHANGE")], "To" = dt$After[which(dt$change == "CHANGE")])

  smrsru_yy <- data.frame("Unit" = change_from_to$Unit, "SMR" = y$smr[which(y$unit %in% change_from_to$Unit)], "SRU" = y$sru[which(y$unit %in% change_from_to$Unit)])
  smrsru_xx <- data.frame("Unit" = change_from_to$Unit, "SMR" = x$smr[which(y$unit %in% change_from_to$Unit)], "SRU" = x$sru[which(y$unit %in% change_from_to$Unit)])

  tab <- cbind(change_from_to, "SRU.1st" = round(smrsru_xx[,3],digits),"SRU.2nd" = round(smrsru_yy[,3],digits))


  if (same){
    tab <- cbind(tab,"SMR.1st" = round(smrsru_xx[,2],digits), "SMR.2nd" = round(smrsru_yy[,2],digits))

    if (compare[1] == "SRU"){
      tab$Rank <- c(rank(tab$SRU.2nd))
      tab <- tab[order(tab$Rank,decreasing = decreasing),]
      tab$Rank <- seq(1,nchanges)
      rownames(tab) <- tab$Rank
    }
    if (compare[1] == "SMR"){
      tab$Rank <- c(rank(tab$SMR.2nd))
      tab <- tab[order(tab$Rank,decreasing = decreasing),]
      tab$Rank <- seq(1,nchanges)
      rownames(tab) <- tab$Rank

    }
    if (compare[1] == "BOTH"){
      tab$Rank <- c(rank(tab$SRU.2nd))
      tab <- tab[order(tab$Rank,decreasing = decreasing),]
      tab$Rank <- seq(1,nchanges)
      rownames(tab) <- tab$Rank
    }

    smrsru_x <- cbind(tab$SMR.1st,tab$SRU.1st)
    smrsru_y <- cbind(tab$SMR.2nd,tab$SRU.2nd)

  } else{
    tab <- cbind(tab, "SMR" = round(smrsru_yy[,2],digits))

    if (compare[1] == "SRU"){
      tab$Rank <- c(rank(tab$SRU.2nd))
      tab <- tab[order(tab$Rank,decreasing = decreasing),]
      tab$Rank <- seq(1,nchanges)
      rownames(tab) <- tab$Rank
    }
    if (compare[1] == "SMR"){
      tab$Rank <- c(rank(tab$SMR))
      tab <- tab[order(tab$Rank,decreasing = decreasing),]
      tab$Rank <- seq(1,nchanges)
      rownames(tab) <- tab$Rank
    }
    if (compare[1] == "BOTH"){
      tab$Rank <- c(rank(tab$SRU.2nd))
      tab <- tab[order(tab$Rank,decreasing = decreasing),]
      tab$Rank <- seq(1,nchanges)
      rownames(tab) <- tab$Rank
    }

    smrsru_x <- cbind(tab$SMR,tab$SRU.1st)
    smrsru_y <- cbind(tab$SMR,tab$SRU.2nd)

  }

  sru_x = x$sru; smr_x = x$smr; sru_y = y$sru; smr_y =  y$smr #all ICUs

  c1 <- 0; c2 <- 0; c3 <- 0; c4 <- 0; c5 <- 0; c6 <- 0; c7 <- 0; c8 <- 0; c9 <- 0; c10 <- 0; c11 <- 0; c12 <- 0;

  quad <- c("OVER", "LE", "UNDER", "ME")

  for(i in 1:nchanges){

    if(change_from_to$From[i] == quad[1] & change_from_to$To[i] == quad[2]){c1 <- c1+1}
    if(change_from_to$From[i] == quad[1] & change_from_to$To[i] == quad[3]){c2 <- c2+1}
    if(change_from_to$From[i] == quad[1] & change_from_to$To[i] == quad[4]){c3 <- c3+1}
    if(change_from_to$From[i] == quad[2] & change_from_to$To[i] == quad[1]){c4 <- c4+1}
    if(change_from_to$From[i] == quad[2] & change_from_to$To[i] == quad[3]){c5 <- c5+1}
    if(change_from_to$From[i] == quad[2] & change_from_to$To[i] == quad[4]){c6 <- c6+1}
    if(change_from_to$From[i] == quad[3] & change_from_to$To[i] == quad[1]){c7 <- c7+1}
    if(change_from_to$From[i] == quad[3] & change_from_to$To[i] == quad[2]){c8 <- c8+1}
    if(change_from_to$From[i] == quad[3] & change_from_to$To[i] == quad[4]){c9 <- c9+1}
    if(change_from_to$From[i] == quad[4] & change_from_to$To[i] == quad[1]){c10 <- c10+1}
    if(change_from_to$From[i] == quad[4] & change_from_to$To[i] == quad[2]){c11 <- c11+1}
    if(change_from_to$From[i] == quad[4] & change_from_to$To[i] == quad[3]){c12 <- c12+1}
  }
  c <- data.frame("OA-LE" = c1, "OA-UA" = c2, "OA-ME" = c3, "LE-OA" = c4, "LE-UA" = c5,"LE-ME" =  c6, "UA-OA" = c7, "UA-LE" = c8, "UA-ME" = c9,"ME-OA" = c10, "ME-LE" = c11, "ME-UA" = c12
  )

  #quantas UTIs melhoram e pioraram com rela??o ao SRU
  better <- sum(c3,c5)
  worse <- sum(c8,c10)
  total <- sum(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12)

  if (better + worse == total){c <- cbind(c, "Melhorou" = better, "Piorou" = worse, "Total" = total)}
  c <- t(c)
  c <- cbind(rownames(c), c)
  colnames(c) <- c("Mov.Quadrante", "Quantidade")

  M_x <- c(median(x$sru), median(x$smr))
  M_y <- c(median(y$sru), median(y$smr))

  Q1_x <- quantile(x$sru, prob = c(.33, .66))
  Q2_x <- quantile(x$smr, prob = c(.33, .66))
  Q1_y <- quantile(y$sru, prob = c(.33, .66))
  Q2_y <- quantile(y$smr, prob = c(.33, .66))

  output <- list(tab = tab, ICU = change_from_to$Unit, smrsru_y = smrsru_y, nchanges = nchanges, smrsru_x = smrsru_x, count = c, med_x = M_x, med_y = M_y, tert_x = c(Q1_x,Q2_x), tert_y = c(Q1_y,Q2_y), sru_x = sru_x, smr_x = smr_x, sru_y = sru_y, smr_y =  smr_y, comp = compare[1])

  class(output) <- "reclass"

  if (plot == TRUE){plot(output)}

  output
}

#' @rdname reclass
#' @export
print.reclass <- function(x, ...){
  print(x$tab, ...)
  cat("------------------------------\n")
  cat("Total:", x$nchanges)
}

#' @rdname reclass
#' @export
plot.reclass <- function(x, ..., xlim = range(x$smr_x), ylim = range(x$sru_x), xlab = "SMR", ylab= "SRU", points.arg_x = list(pch = 21, col = "white", bg = "yellow", cex = 2), points.arg_y = list(pch = 21, col = "white", bg = "yellow", cex = 2), med.arg_x = list(col = "dodgerblue4",lwd = 2,lty = 1), med.arg_y = list(col = "dodgerblue4", lwd = 2, lty = 1), tert.arg_x = list(col = "darkorange2", lty = 2, lwd = 1), tert.arg_y = list(col = "darkorange2", lty = 2, lwd = 1), changes.arg_x = list(pch = 21, col = "white", bg = "yellow", cex = 2), changes.arg_y = list(pch = 21, col = "white", bg = "yellow", cex = 2), text.arg_x = list(labels = x$tab$Rank, cex=.6), text.arg_y = list(labels = x$tab$Rank, cex=.6), worse.arg_x = list(pch = 21, col = "white", bg = "tomato", cex = 2), worse.arg_y = list(pch = 21, col = "white", bg = "tomato", cex = 2), better.arg_x = list(pch = 21, col = "white", bg = "mediumseagreen", cex = 2), better.arg_y = list(pch = 21, col = "white", bg = "mediumseagreen", cex = 2), auto.legend = TRUE, leg.arg = list(x = "bottomleft", bty = "n", xpd = T, inset = -.35, ncol = 1, horiz = F, pch = 19, cex = .8, pt.cex = 1.5), main.arg_x = list(main = "1? Stage"), main.arg_y = list(main = "2? Stage")){
  med.arg_x$v <- x$med_x[2]
  med.arg_x$h <- x$med_x[1]
  med.arg_y$v <- x$med_y[2]
  med.arg_y$h <- x$med_y[1]
  tert.arg_x$h <- x$tert_x[1:2]
  tert.arg_x$v <- x$tert_x[3:4]
  tert.arg_y$h <- x$tert_y[1:2]
  tert.arg_y$v <- x$tert_y[3:4]
  points.arg_x$x <- cbind(x$smr_x, x$sru_x)
  points.arg_y$x <- cbind(x$smr_y, x$sru_y)
  changes.arg_x$x <- x$smrsru_x
  changes.arg_y$x <- x$smrsru_y
  text.arg_x$x <- x$smrsru_x
  text.arg_y$x <- x$smrsru_y
  worse.arg_x$x <- matrix(nc = 2, dimnames = list(NULL, c("SMR", "SRU")))
  worse.arg_y$x <- matrix(nc = 2, dimnames = list(NULL, c("SMR", "SRU")))
  better.arg_x$x <- matrix(nc = 2, dimnames = list(NULL, c("SMR", "SRU")))
  better.arg_y$x <- matrix(nc = 2, dimnames = list(NULL, c("SMR", "SRU")))

  if (x$comp == "SRU"){
    for (i in 1:x$nchanges){
      if (x$tab[,2][i] == "ME" && x$tab[,3][i] == "LE" | x$tab[,2][i] == "ME" && x$tab[,3][i] =="UNDER" | x$tab[,2][i] == "OVER" && x$tab[,3][i] == "LE" | x$tab[,2][i] == "OVER" && x$tab[,3][i] == "UNDER" | x$tab[,2][i] == "UNDER" && x$tab[,3][i] == "LE" ){
        worse.arg_x$x <- rbind(worse.arg_x$x, x$smrsru_x[i,])
        worse.arg_y$x <- rbind(worse.arg_y$x, x$smrsru_y[i,])
      }
      if (x$tab[,2][i] == "LE" && x$tab[,3][i] == "OVER" | x$tab[,2][i] == "LE" && x$tab[,3][i] == "ME" | x$tab[,2][i] == "UNDER" && x$tab[,3][i] == "ME" | x$tab[,2][i] == "UNDER" && x$tab[,3][i] == "OVER"){
        better.arg_x$x <- rbind(better.arg_x$x, x$smrsru_x[i,])
        better.arg_y$x <- rbind(better.arg_y$x, x$smrsru_y[i,])
      }
    }
  }

  if (x$comp == "SMR"){
    for (i in 1:x$nchanges){
      if (x$tab[,2][i] == "ME" && x$tab[,3][i] == "OVER" | x$tab[,2][i] == "ME" && x$tab[,3][i] == "LE" | x$tab[,2][i] == "UNDER" && x$tab[,3][i] == "LE" | x$tab[,2][i] == "UNDER" && x$tab[,3][i] == "OVER"){
        worse.arg_x$x <- rbind(worse.arg_x$x, x$smrsru_x[i,])
        worse.arg_y$x <- rbind(worse.arg_y$x, x$smrsru_y[i,])
      }
      if (x$tab[,2][i] == "OVER" && x$tab[,3][i] == "ME" | x$tab[,2][i] == "OVER" && x$tab[,3][i] == "UNDER" | x$tab[,2][i] == "LE" && x$tab[,3][i] == "ME" | x$tab[,2][i] == "LE" && x$tab[,3][i] == "UNDER"){
        better.arg_x$x <- rbind(better.arg_x$x, x$smrsru_x[i,])
        better.arg_y$x <- rbind(better.arg_y$x, x$smrsru_y[i,])
      }
    }
  }

  if (x$comp == "BOTH"){
    for (i in 1:x$nchanges){
      if (x$tab[,2][i] == "ME" && x$tab[,3][i] == "OVER" | x$tab[,2][i] == "ME" && x$tab[,3][i] == "LE" | x$tab[,2][i] == "ME" && x$tab[,3][i] == "UNDER" | x$tab[,2][i] == "OVER" && x$tab[,3][i] == "LE" | x$tab[,2][i] == "UNDER" && x$tab[,3][i] == "LE" ){
        worse.arg_x$x <- rbind(worse.arg_x$x, x$smrsru_x[i,])
        worse.arg_y$x <- rbind(worse.arg_y$x, x$smrsru_y[i,])
      }
      if (x$tab[,2][i] == "OVER" && x$tab[,3][i] == "ME" | x$tab[,2][i] == "LE" && x$tab[,3][i] == "ME" | x$tab[,2][i] == "UNDER" && x$tab[,3][i] == "ME"  | x$tab[,2][i] == "LE" && x$tab[,3][i] == "UNDER" | x$tab[,2][i] == "LE" && x$tab[,3][i] == "ME" | x$tab[,2][i] == "LE" && x$tab[,3][i] == "OVER" | x$tab[,2][i] == "UNDER" && x$tab[,3][i] == "ME" ){
        better.arg_x$x <- rbind(better.arg_x$x, x$smrsru_x[i,])
        better.arg_y$x <- rbind(better.arg_y$x, x$smrsru_y[i,])
      }
    }
  }

  worse.arg_x$x <- worse.arg_x$x[-1,]
  worse.arg_y$x <- worse.arg_y$x[-1,]
  better.arg_x$x <- better.arg_x$x[-1,]
  better.arg_y$x <- better.arg_y$x[-1,]

  par(mfrow = c(1,2))
  plot(0, 0, ..., xlim = xlim, ylim = ylim, type = 'n', xlab = xlab, ylab = ylab, main = main.arg_x[[1]])
  do.call(abline, med.arg_x)
  do.call(abline, tert.arg_x)
  do.call(points, points.arg_x)
  do.call(points, changes.arg_x)
  do.call(points, worse.arg_x)
  do.call(points, better.arg_x)
  do.call(text, text.arg_x)

  plot(0, 0, ..., xlim = xlim, ylim = ylim, type = 'n', xlab = xlab, ylab = ylab, main = main.arg_y[[1]])
  do.call(abline, med.arg_y)
  do.call(abline, tert.arg_y)
  do.call(points, points.arg_y)
  do.call(points, changes.arg_y)
  do.call(points, worse.arg_y)
  do.call(points, better.arg_y)
  do.call(text, text.arg_y)
  if(auto.legend){
    leg.arg$legend <- c("Worse rank", "Better rank", "Unchanged rank")
    leg.arg$col <- c(worse.arg_y$bg, better.arg_y$bg, points.arg_y$bg)
    do.call(legend, leg.arg)
  }
}
