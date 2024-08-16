#' Formatting function.
#'
#' @param x value to format
#'
#' @importFrom magrittr %>%
#'
#' @export

Formatting <- function(x) {
  new_strs <- c()
  for (i in 1:length(x)) {
    substrs <- strsplit(x[i], split = "E")[[1]]
    if (abs(as.numeric(substrs[1]) * 10^as.numeric(substrs[2])) < 10^(-15)) {
      new_strs <- c(new_strs, "$\\mathbf{< 1.000 \\times 10^{-15}}$")
    } else if (as.numeric(substrs[2]) == 0) {
      new_strs <- c(new_strs, paste0("$", substrs[1], "$"))
    } else if (as.numeric(substrs[2]) == 1) {
      new_strs <- c(new_strs, paste0("$", substrs[1], " \\times 10$"))
    } else if (as.numeric(substrs[2]) == -1) {
      new_strs <- c(new_strs, paste0("$", format(as.numeric(substrs[1]) / 10, digits = 3, nsmall = 3), "$"))
    } else {
      new_strs <- c(new_strs, paste0("$", substrs[1], " \\times 10^{", as.numeric(substrs[2]), "}$"))
    }
  }
  return (new_strs)
}
