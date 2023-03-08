#' Create a formula from column names of a df
#'
#' @param y A character vector with one element. The name of the response variable.
#' @param dframe A data frame like object with the named columns to use in the formula.
#'
#' @return A formula.
#'
#' @importFrom stats as.formula
#' @export
#'
#' @examples
#' dframe <- data.frame(y=c(1, 1, -1), x1=c("a", "b", "c"), x2=runif(3), x3=rnorm(3))
#' fmla_from_names("y", dframe)
fmla_from_dframe <- function(y, dframe) {
  nims <- names(dframe)
  fmla <- as.formula(paste(y, "~", paste(nims[!nims %in% y], collapse = " + ")))
  return(fmla)
}

#' Create a formula from names
#'
#' @param y A character vector with one element. The name of the response variable.
#' @param nims A character vector of any length. The names of the independent variables or features.
#'
#' @return A formula.
#'
#' @importFrom stats as.formula
#' @export
#'
#' @examples
#' nims <- c("x1", "x2", "x3")
#' fmla_from_names("y", nims)
fmla_from_names <- function(y, nims) {
  fmla <- as.formula(paste(y, "~", paste(nims, collapse = " + ")))
  return(fmla)
}
