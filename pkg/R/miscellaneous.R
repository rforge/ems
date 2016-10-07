#' Miscellaneous functions for data editing
#'
#' @name miscellaneous
#'
#' @description Collections of functions for data editing ussually used as lower levels for other functions.
#'
#' \code{f.num} is a wrapper to format numeric variables that are stored as character or factor, simultaneously it will try to detect comma spearated and replace it by dots before formating the variable as numeric.
#'
#' @param num.var A character, or factor variable to be formated as numeric.
#'
#' @examples
#'
#' f.num(c("2,4000","10,0000","5.0400"))
#'
#' @rdname miscellaneous
#' @export
f.num <- function(num.var){
  if(!any(class(num.var) %in% c("numeric","integer","double"))){
    if(any(is.character(num.var))){
      if(any(grepl(",",num.var))){num.var <- sub(",", ".", num.var)}
      output <- suppressWarnings(as.numeric(num.var))
    }
    if(any(is.factor(num.var))){
      if(any(grepl(",",levels(num.var)))){levels(num.var) <- sub(",", ".",levels(num.var))}
      output <- suppressWarnings(as.numeric(as.character(num.var)))
    }
  } else {output <- num.var}
  output
}
