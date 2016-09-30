#' Standardized Mortality Ratio (SMR)
#'
#' @name SMR
#'
#' @description Calculates the standardized mortality ratio and its confidence interval. SMR, for a group, is defined as the ratio of the observed deaths in this group and the sum of the predicted individual probabilities of death by any model.
#'
#' \code{SMR.table} estimate at once the overall SMR and the SMR across several groups, e.g. ICU units or clinical characteristics. The \code{SMR.table} can be ordered by the SMR estimate or its confidence intervals, facilitating the comparinson of the units ranks.
#'
#' \code{forest.SMR} shows the \code{SMR.table} output as a forest plot. The plot opens two windows and plot at the left side the values from the \code{SMR.table} and at the right side the points and lines graphically representing each SMR and its confidence interval.
#'
#' @param obs.var Observed death. Accepted values are 0 (absence) or 1 (presence) in a vector. For \code{SMR.table} is must be a character indicating the name of the variable in data.
#'
#' @param pred.var Death individual predictions (ranging from 0 to 1) in a vector. For \code{SMR.table} is must be a character indicating the name of the variable in data.
#'
#' @param digits Number of digits for rounding the output.
#'
#' @param ci.method Method to estimate the confidence interval. "Hosmer" (default) or "Byar" are acceptable values.
#'
#' @param ci.level Level of the confidence interval. Default is 0.95.
#'
#' @param data A dataset where pred.var, obs.var and group.var are in.
#'
#' @param group.var For \code{SMR.table} is must be a vector indicating the name(s) of the variable(s) in data indicating which variables will form the groups across which SMR will be calculated. Must be factor variables.
#'
#' @param use.label Logical. Default is TRUE. For \code{SMR.table} this option will replace the variables names by its lables in var.labels argument.
#'
#' @param var.labels A character vector with variables labels. The default is to replace the variable name by the label stored at attr(data, "var.labels "). But one may specify labels directly.
#'
#' @param reorder Default is "no". Possible values are: "no", "SMR","lower.Cl", and "upper.Cl". Will make the \code{SMR.table} to be ordered within each varibale by its original order, or by SMR order, or by lower.Cl order, or by upper.Cl.
#'
#' @param decreasing Logical. Should the order be decreasing or incresing. See \code{\link[base]{order}}
#'
#' @param x For the \code{forest.SMR} this is the output of \code{SMR.table}.
#'
#' @param mar1,mar.SMR Values to set the margins (mar parameter) of left and right windows. See \code{\link[graphics]{par}}
#'
#' @param overall.arg A list of arguments passed to \code{\link[graphics]{text}} for ploting the overall label. Internally  and 'y' coordinate is replaced.
#'
#' @param NOE.args A list of arguments passed to \code{\link[graphics]{text}} for ploting the overall N (number of observations), O (observed eaths) and E (expected deaths). Internally 'labels' and 'y' arguments are replaced.
#'
#' @param var.labels.arg A list of arguments passed to \code{\link[graphics]{text}} for ploting the variables labels. Internally  and 'y' coordinate is replaced.
#'
#' @param var.labels.arg A list of arguments passed to \code{\link[graphics]{text}} for ploting the categories labels. Internally  and 'y' coordinate is replaced.
#'
#' @return
#' If SMR, then:
#' \itemize{
#' \item \code{N} Number of subjects analyzed.
#' \item\code{O} Observed number of deaths.
#' \item \code{E} Expected number of deaths.
#' \item \code{SMR} Standardized mortality ratio.
#' \item \code{lower.Cl} lower confidence limit.
#' \item \code{upper.Cl} upper confidence limit.
#' }
#'
#' If SMR.table, then a data.frame with the the same information as above, and the additional information: "Variables" (variables names), "Levels" (variables levels).
#'
#'If forest.SMR, then a plot is returned.
#' @references
#'
#' @author Lunna Borges and Pedro Brasil
#'
#' @seealso \link{SRU}
#'
#' @examples
#'
#' @import stats graphics
#' @export
SMR <- function(obs.var, pred.var, digits = 5, ci.method = c("Hosmer", "Byar"), ci.level = 0.95) {
  if (length(obs.var) != length(pred.var)){
    stop("Length of pred.var and obs.var differ.")
  }
  if (is.na(obs.var) || is.na(pred.var)) {
    stop("There are NAs either at pred.var or obs.var. Either remove the NAs or impute!")
  }
  if (!is.numeric(pred.var)) {
    stop("Predicted death variable must be numeric.")
  }
  if (any(min(pred.var) < 0 | max(pred.var) > 1)) {
    stop("The individual predicted death must range from 0 to 1.")
  }
  if (!is.numeric(obs.var)) {
    stop("Observed death variable must be numeric.")
  }
  if (all(obs.var != 0) && all(obs.var != 1)) {
    stop("Observed death variable must be coded as 0 and 1.")
  }
  if (ci.method[1] != "Byar" && ci.method[1] != "Hosmer") {
    stop("ci.method must be either 'Byar' or 'Hosmer'.")
  }
  if (!is.numeric(ci.level) || ci.level > 1 || ci.level < 0) {
    stop("ci.level must be numeric between 0 and 1.")
  }
  N <- length(pred.var)
  O <- length(obs.var[which(obs.var == 1)])
  E <- sum(pred.var)
  SMR <- O / E
  if (ci.method[1] == "Byar") {
    lowerCL <- O / E * (1 - 1 / (9 * O) - qnorm(1 - (1 - ci.level) / 2) / (3 * sqrt(O))) ^ 3
    upperCL <- (O + 1) / E * (1 - (1 / (9 * (O + 1))) + qnorm(1 - (1 - ci.level) / 2) /
                                (3 * sqrt(O + 1))) ^ 3
  }
  if (ci.method[1] == "Hosmer") {
    V2 <- sum(pred.var * (1 - pred.var))
    lowerCL <- exp(log(SMR) - qnorm(1 - (1 - ci.level) / 2) * sqrt(V2 / O ^ 2))
    upperCL <- exp(log(SMR) + qnorm(1 - (1 - ci.level) / 2) * sqrt(V2 / O ^ 2))
  }
  output <- c(N = N, Observed = O, Expected = E, SMR = SMR, lower.Cl = lowerCL,
              upper.Cl = upperCL)
  output <- round(output, digits = digits)
  output
}

#' @rdname SMR
#' @export
SMR.table <- function(data, group.var, obs.var, pred.var, digits = 5, use.label = TRUE, var.labels = attr(data, "var.labels")[match(group.var, names(data))], ci.method = c("Hosmer", "Byar"), ci.level = 0.95, reorder = c("no","SMR","lower.Cl","upper.Cl"), decreasing = FALSE) {
  if (any(is.na(match(group.var, names(data))))) {
    stop("One or more variables in group var is not a variable of data.")
  }
  if (!all(sapply(data[ , group.var], is.factor))) {
    stop("All variables in group.var must be factors.")
  }
  if (use.label) {
    if (is.null(var.labels )) {
      stop("Either there is no label in attr(data,'var.labels') or 'var.labels argument was not set.")
    }
    if (length(group.var) != length(var.labels )){
      stop("The number of variables and labels are different.")
    }
    if (!is.character(var.labels )) {
      stop("'var.labels ' must be a character vector.")
    }
  }
  if (reorder !=  "no" && reorder !=  "SMR" && reorder !=  "lower.Cl" && reorder !=  "upper.Cl") {
    stop("'reorder' must be one of 'no','SMR','lower.Cl' or 'upper.Cl'")
  }
  Variables <- c("Overall", unlist(sapply(1:length(group.var), function(i) rep(group.var[i], nlevels(data[ , group.var[i]])))))
  if (length(group.var) == 1) {
    Levels <- c(NA, levels(data[ , group.var]))
  } else {
    Levels <- c(NA, unname(unlist(sapply(data[ , group.var], levels))))
  }
  x <- data.frame(Variables, Levels)
  x$N <- NA
  x$Observed <- NA
  x$Expected <- NA
  x$SMR <- NA
  x$lower.Cl <- NA
  x$upper.Cl <- NA
  x[1,3:8] <- SMR(data[ , obs.var], data[, pred.var], digits = digits, ci.method = ci.method, ci.level = ci.level)
  for(i in 2:nrow(x)){
    cond <- which(data[ , as.character(x[i , "Variables"])] == as.character(x[i , 2]))
    x[i,3:8] <- SMR(data[cond , obs.var], data[cond , pred.var], digits = digits, ci.method = ci.method, ci.level = ci.level)
  }
  x$Variables <- as.character(x$Variables)
  if (reorder[1] == "SMR") {
    for (i in group.var) {
      # i = 1
      cond <- grep(i, x$Variables)
      y <- x[ cond, ]
      y <- y[ order(y$SMR, decreasing = decreasing), ]
      x[ cond, ] <- y
    }
  }
  if (reorder[1] == "lower.Cl") {
    # i = 2
    for (i in group.var) {
      cond <- grep(i, x$Variables)
      y <- x[ cond, ]
      y <- y[ order(y$lower.Cl, decreasing = decreasing), ]
      x[ cond, ] <- y
    }
  }
  if (reorder[1] == "upper.Cl") {
    for (i in group.var) {
      cond <- grep(i, x$Variables)
      y <- x[ cond, ]
      y <- y[ order(y$upper.Cl, decreasing = decreasing), ]
      x[ cond, ] <- y
    }
  }
  if (use.label) {
    names(group.var) <- var.labels
    for (i in 2:length(x$Variables)) {
           x$Variables[i] <- names(group.var)[which(x$Variables[i] == group.var)]
    }
  }
  for (i in length(x$Variables):2) {
    if (x$Variables[i - 1] == x$Variables[i]) {x$Variables[i] <- NA}
  }
  x
}

#' @rdname SMR
#' @export
############## Essa fun??o cria um grafico em floresta a partir do SMR.table
# x ? a saida da fun??o SMR.table
# mfrow (ver par) - ignorado no momento (em algum momento achei que pudessem entrar SMR de mais de um modelo)
# mar1 (ver mar em par) - margens da primeira janela
# mar.SMR - margens da segunda janela
# seg.col = cor da linhas horizontais do IC dos SMR
# pch s?o o tipo cor e tamanho da estimativa pontual do SMR
# cex.var e cex.cat s?os tamanhos das fontes das variaveis e das catagorias
# var.col e cat.col s?os cores das fontes das variaveis e das catagorias
# x.var e x.cat, x.observed e x.expected s?o as posi??es das colunas no eixo horizontal
# adj ? adj que passa pra text na variavel e categorias
# smr.xlim s?o os limites dos eixo horizontal
# smr.pos ? o quanto que o textos de estimativas de SMR afastam do limite esquerdo da janela com o grafico
forest.SMR <- function(x,
                       mar1 = c(5.1, 1, 4.1, 1),
                       mar.SMR = c(5.1, 7, 4.1, 1),
                       overall.arg = list(x = .01, font = 2, las = 1, cex = 1, labels = var.labels[1], xpd = NA, adj = 0),
                       NOE.args = list(x = c(.5, .675, .85), font = 2, las = 1, cex = 1, xpd = NA),
                       var.labels.arg = list(x = .01, font = 2, las = 1, cex = 1, xpd = NA, adj = 0),
                       cat.labels.arg = list(x = .1, font = 3, las = 1, cex = .95,  col = "gray30", xpd = NA , adj = 0),
                       smr.xlab = "Standardized Mortality Ratio",
                       seg.col="blue",
                       seg.lty = 1,
                       seg.lwd = 1,
                       pch.type = 18,
                       pch.cex = 2,
                       pch.col = 1,
                       cex.var = 1,
                       cex.cat = .95,
                       cat.col = "gray30",
                       var.col = "black",
                       font.var = 2,
                       font.cat = 3,
                       x.var = .01,
                       x.cat = .1,
                       x.n = .50,
                       x.observed = .675,
                       x.expected = .85,
                       adj.var = 0,
                       adj.cat = 0,
                       adj.smr = 1,
                       smr.xlim = "auto",
                       smr.pos = .06,
                       grid = TRUE,
                       digits = 3){
  on.exit(par())

  # Separando os valores da medida preincipal
  main.pos = 1
  main.smr = unlist(x[which(x$Variables == "Overall"), c("SMR", "lower.Cl", "upper.Cl")])
  main.n = unlist(x[which(x$Variables == "Overall"), "N"])
  main.observed = round(unlist(x[which(x$Variables == "Overall"), "Observed"]), digits = digits)
  main.expected = round(unlist(x[which(x$Variables == "Overall"), "Expected"]), digits = digits)

  # Separando as medidas do SMR para montar o grafico
  smr.estimates = unlist(x$SMR[-1])
  smr.ll = unlist(x$lower.Cl[-1])
  smr.ul = unlist(x$upper.Cl[-1])

  # Separando os valores para serem impressos na direita do grafico
  N <- unlist(x$N[-1])
  observed <- round(unlist(x$Observed[-1]), digits = digits)
  expected <- round(unlist(x$Expected[-1]), digits = digits)

  # Achandos os limites do grafico no eixo horizontal
  if (smr.xlim == "auto"){
    smr.xlim = c(min(unlist(c(main.smr[2], smr.ll)), na.rm=T), max(unlist(c(main.smr[3], smr.ul)), na.rm=T))
  }

  # Separando os valores das vari?veis e categorias
  var.labels = x$Variables[-which(is.na(x$Variables))]
  cat.labels = x$Levels[-which(is.na(x$Levels))]

  # Achando as posi??es no eixo vertical
  nLevels <- c(which(!is.na(x$Variables))[-1],(nrow(x) + 1))
  for(i in 1:length(nLevels)){
    nLevels[i] <- nLevels[i+1] - nLevels[i]
  }; rm(i)
  nLevels <- nLevels[which(!is.na(nLevels))]
  # x$Levels
  # i = 3
  cat.pos <- seq(1:nLevels[1])
  for(i in 2:length(nLevels)){
    ini <- cat.pos[length(cat.pos)]
    cat.pos <- c(cat.pos, (ini + 3):(ini + 2 + nLevels[i]))
  }
  cat.pos <- cat.pos + 2

  var.pos = nLevels[1] + 1
  # i = 2
  for(i in 2:length(nLevels)){
    var.pos <- c(var.pos, var.pos[i-1] + nLevels[i] + 2)
  }
  var.pos <- var.pos + 2
  ylim = c(1, var.pos[length(var.pos)])
  # 5.1 4.1 4.1 2.1

  par(mfrow = c(1,2))

  # Janela com textos com vari?veis e categorias
  par(mar = mar1)
  plot(NA, NA, xlim = c(0,1), ylim = ylim , xlab = "", ylab = "",yaxt = "n", xaxt = "n", xaxs = "i", bty = "n", xpd = NA)

  # Efeito principal
  overall.arg$y <- main.pos
  do.call(text, overall.arg)

  # N O E do overall
  NOE.args$labels <- c(main.n, main.observed, main.expected)
  NOE.args$y <- main.pos
  do.call(text, NOE.args)

  # Demais vari?veis
  var.labels.arg$labels <- var.labels[2:length(var.labels)]
  var.labels.arg$y <- var.pos
  do.call(text, var.labels.arg)

  # Categorias
  cat.labels.arg$labels <- cat.labels
  cat.labels.arg$y <- cat.pos
  do.call(text, cat.labels.arg)

  # Colunas com os valores de N O e E
  # NOE.values.arg <-
  text(x.n, cat.pos, las = 1, cex = cex.var,  col = cat.col, labels = N, xpd = NA)
  text(x.observed, cat.pos, las = 1, cex = cex.var,  col = cat.col, labels = observed, xpd = NA)
  text(x.expected, cat.pos, las = 1, cex = cex.var,  col = cat.col, labels = expected, xpd = NA)

  # Cabe?alho das colunas N O E
  text(c(x.n,x.observed,x.expected), ylim[2] + 1, font = font.var, cex = cex.var,  col = var.col, labels = c("N","O","E"), xpd = NA)

  # SMR
  par(mar = mar.SMR)
  plot(NA, NA, xlim = smr.xlim, ylim = ylim , xlab = smr.xlab, ylab = "",yaxt = "n", xaxt = "n", xaxs = "i", bty = "n", xpd = NA);axis(1)
  if(grid == T) grid()

  # Efeito principal
  segments(main.smr[2], main.pos, main.smr[3], main.pos,col = seg.col, lty = seg.lty, lwd = seg.lwd, xpd = NA)
  points(main.smr[1], main.pos, type = "p", pch = pch.type ,cex = pch.cex, col = pch.col, xpd = NA)
  text(smr.xlim[1] - smr.pos, main.pos, las = 1, cex = cex.var, font = font.var, col = cat.col, labels = sprintf("%.3f [%.3f ; %.3f]",main.smr[1],main.smr[2],main.smr[3]), xpd = NA, adj = adj.smr)

  # Das demais vari?veis
  segments(smr.ll, cat.pos, smr.ul, cat.pos,col = seg.col, lty = seg.lty, lwd = seg.lwd, xpd = NA)
  points(smr.estimates, cat.pos, type = "p", pch = pch.type ,cex= pch.cex, col = pch.col, xpd = NA)
  text(smr.xlim[1] - smr.pos, cat.pos, las = 1, cex = cex.var,  col = cat.col, labels = sprintf("%.3f [%.3f ; %.3f]",smr.estimates,smr.ll,smr.ul), xpd = NA, adj = adj.smr)
  text(smr.xlim[1] - smr.pos, ylim[2] + 1, font = font.var, cex = cex.var,  col = var.col, labels = "SMR [95% CI]", xpd = NA, adj = adj.smr)
}

