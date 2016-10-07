#' Collection of function to check data quality in a dataset
#'
#' @name dataquality
#'
#' @description These functionsn return the counts and fractions of expected values, unexpected values, missing values and non valid values.
#'
#' \code{t_factor} and \code{factor.table} will try to get factor or character variables and check how much of its content match with the expectd. They will try to treat the levels or cells with "" as NAs.
#'
#' \code{t_num} will try to get a numeric variable and check how much of its content are expected (match a desired range), unexpected, nonnumeric values and missing vlaues. \code{num.table} does the same thing, but with two or more variables at once.
#'
#' @param data A data.frame where variables will be tested.
#'
#' @param variable Acharacter vector of length one, indicating the variable name in dataset to be tested.
#'
#' @param legal A character vector representeing the expected levels of the tested variable.
#'
#' @param limits a list of two or more lists, each containing the arguments variable and legal (in this order). See examples.
#'
#' @param var.labels Variables labels to nice output. Must be iformed in the same order as variable argument. By default, it captures the labels stored in attr(data, "var.labels"), if any.
#'
#' @param num.var A character vector indicating the name of a variable that should be numeric (althoug it can yet be formated as character or factor).
#'
#' @param num.max,num.min The limits of acceptable range of a numeric variable.
#'
#' @param num.limits A data.frame with the following variables: num.vars, num.max and num.min. See example.
#'
#' @param digits Decimal for rounding
#'
#' @examples
#' # Simulating a dataset with 4 factor variables and assigning labels
#' y <- data.frame(Var1 = sample(c("Yes","No", "Ignored", "", "yes ", NA), 200, replace = TRUE),
#'                 Var2 = sample(c("Death","Discharge", "", NA), 200, replace = TRUE),
#'                 Var3 = sample(c(16:35, NA), 200, replace = TRUE),
#'                 Var4 = sample(c(12:300, "Female", "", NA), 200, replace = TRUE))
#' attr(y, "var.labels") <- c("Intervention use","Unit detination","BMI","Age")
#'
#' # Cheking the quality only the first variable
#' t_factor(y, "Var1", c("Yes","No","Ignored"))
#'
#' # Checkin two or more variables at once
#' factor.table(y, limits = list(
#'                           list("Var1",c("Yes","No")),
#'                           list("Var2",c("Death","Discharge"))))
#'
#' # Checking only one "numeric" variable
#' t_num(y,"Var3", num.min = 17, num.max = 32)
#'
#' # Making the limits data.frame
#' num.limits <- data.frame(num.vars = c("Var3","Var4"),
#'               num.min = c(17,18), num.max = c(32,110))
#' num.limits
#'
#' # Checkin ntwo or more numeric variables at once
#' num.table(y, num.limits)
#'
#' rm(y)
#'
#' @export
t_factor <- function(data, variable, legal, var.labels= attr(data, "var.labels")[match(variable, names(data))], digits = 3){
  if (!is.data.frame(data)) {
    stop("Argument 'data' is not a data.frame.")
  }
  if (!is.character(variable)) {
    stop("Argument 'variable' is not a character vector.")
  }
  if (length(variable) != 1) {
    stop("'t_factor' accepts one 'variable' at the time.")
  }
  if (!(variable %in% names(data))) {
    stop(paste(variable, "is not a varaible in", substitute(data)))
  }
  if (any(is.null(var.labels))) {
    var.labels <- variable
  }
  var <- data[ , variable]
  var.dim <- length(var)
  na.sum <- table(is.na(var))[2]
  if (is.na(na.sum)) {na.sum <- 0}
  tmp <- !(var %in% legal)
  table.tmp <- table(tmp)
  if (any(names(table.tmp) == "FALSE")) {
    val.esp <- table.tmp[which(names(table.tmp) == "FALSE")]
  } else {val.esp <- 0}
  if (any(is.na(var))) {
    tmp[which(is.na(var))] <- FALSE
  }
  table.tmp <- table(tmp)
  if (any(names(table.tmp) == "TRUE")) {
    val.n.esp <- table.tmp[which(names(table.tmp) == "TRUE")]
  } else {val.n.esp <- 0}
  output <- c(var.labels , paste0(val.esp, " (", sprintf(paste0("%.",digits,"f"), val.esp / var.dim,digits), ")"),
              paste0(val.n.esp, " (", sprintf(paste0("%.",digits,"f"), val.n.esp/var.dim,digits), ")"),
              paste0(na.sum, " (", sprintf(paste0("%.",digits,"f"), na.sum/var.dim,digits), ")"))
  names(output) <- c("Variable","Expected values","Unexpected values","Missing values")
  output
}

#' @rdname dataquality
#' @export
factor.table <- function(data, limits, var.labels = attr(data, "var.labels")[match(unlist(sapply(seq_along(limits), function(i) limits[[i]][1])), names(data))], digits = 3){
  if (!is.data.frame(data)) {
    stop("Argument 'data' is not a data.frame.")
  }
  if (!is.list(limits)) {
    stop("Argument 'limits' is not a list.")
  }
  var <- unlist(sapply(seq_along(limits), function(i) limits[[i]][1]))
  if (!any(var %in% names(data))) {
    stop(paste0(var[-which(var %in% names(data))]," are not in the dataset"))
  }
  # cat("Factor variables analysed: \n")
  # cat(var, fill = length(var),"\n")
  output <- as.data.frame(t(sapply(seq_along(limits), function(i) t_factor(data, var[i], unlist(limits[[i]][2]), digits = digits, var.labels = var.labels[i]))))
  output
}

#' @rdname dataquality
#' @export
t_num <- function(data, num.var, num.max = 100, num.min = 0, var.labels= attr(data, "var.labels")[match(num.var, names(data))], digits = 3){
  if (num.max < num.min) { stop("num.max is lower than num.min.") }
  if (any(is.null(var.labels))) {
    var.labels <- num.var
  }
  if (length(num.var) != 1) {
    stop("'t_num' accepts one 'num.var' at the time.")
  }
  if (!(num.var %in% names(data))) {
    stop(paste(toString(num.var), "is not a varaible in", substitute(data)))
  }
  num.var <- data[,num.var]
  num.dim <- length(num.var)
  na.sum <- table(is.na(num.var))[2]
  if(is.na(na.sum)){na.sum <- 0}
  fnum <- f.num(num.var)
  #### head(fnum);head(num.var)
  val.n.num <- sum(table(num.var[which(is.na(fnum))]))
  # a <- a[order(date),]
  val.n.esp <- table(ifelse(fnum > num.max | fnum < num.min,1,0))
  if(any(names(val.n.esp) == "0")){val.esp <- val.n.esp[which(names(val.n.esp) == "0")]} else {val.esp <- 0}
  if(any(names(val.n.esp) == "1")){val.n.esp <- val.n.esp[which(names(val.n.esp) == "1")]}  else {val.n.esp <- 0}
  output <- c(var.labels, paste0(val.esp," (",sprintf(paste0("%.",digits,"f"), val.esp/num.dim),")"),
              paste0(val.n.esp," (", sprintf(paste0("%.",digits,"f"),val.n.esp/num.dim),")"),
              paste0(val.n.num," (", sprintf(paste0("%.",digits,"f"), val.n.num/num.dim),")"),
              paste0(na.sum," (", sprintf(paste0("%.",digits,"f"), na.sum/num.dim),")"))
  names(output) <- c("Variable","Expected values","Unexpected values","Non-numeric values","Missing values")
  output
}

#' @rdname dataquality
#' @export
num.table <- function(data, num.limits, var.labels = attr(data, "var.labels")[match(num.limits$num.vars, names(data))], digits = 3){
  if (!is.data.frame(data)) {
    stop("Argument 'data' is not a data.frame.")
  }
  if (!is.data.frame(num.limits)) {
    stop("Argument 'num.limits' is not a data.frame.")
  }
  if (!all(names(num.limits) %in% c("num.vars","num.max","num.min"))) {
    stop("'num.limits' must be a data.frame with the following columns: 'num.vars', 'num.min' and 'num.min'.")
  }
  if (!(is.character(num.limits$num.vars) | is.factor(num.limits$num.vars))) {
    stop("'num.limits$num.vars' must be a character vector.")
  }
  if (!is.numeric(num.limits$num.min) || !is.numeric(num.limits$num.max)) {
    stop("'num.limits$num.min' and 'num.limits$num.max' must be numeric vectors.")
  }
  if (!any(num.limits$num.vars %in% names(data))) {
    stop(paste0(var[-which(num.limits$num.vars %in% names(data))]," are not in the dataset"))
  }
  if (!any(num.limits$num.vars %in% names(data))) {
    stop(paste0(num.limits$num.vars[-which(num.limits$num.vars %in% names(data))]," are not in the dataset"))
    }
  if (any(num.limits$num.max < num.limits$num.min)) {
    stop(paste0("num.max is lower than num.min in ",num.limits$num.vars[which(num.limits$num.max < num.limits$num.min)]))
  }
  num.limits$num.vars <- as.character(num.limits$num.vars)
  output <- as.data.frame(t(sapply(1:nrow(num.limits), function(i) t_num(data = data, num.var = num.limits$num.vars[i], num.max = num.limits$num.max[i], num.min = num.limits$num.min[i], digits = digits, var.labels = var.labels[i]))))
  output
}

