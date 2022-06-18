#' Summarise the results of 'mmcsd' fit.
#'
#' @param object A mmcsd fitted model
#' @param ... Additional params passed to summary
#' @return Return NULL and print in terminal the results.
#'
#' @export
#'
summary.mmcsd <- function(object, ...) {
  coefficientsTable <- createCoefficientsTable(object)

  catCall(object)
  catCoefficientsTable(coefficientsTable, object)
  catSampleDesign(object)
  catSigma(object)
  catSigmaHat(object)

  return(invisible(NULL))
}
