#' covariance structure viewer to preview sigmaThetaExpr to be used in 'cov_mmcsd'.
#' 
#' Knowing the difficulty of visualizing the covariance structure, especially when the user chooses to determine his own structure. This function was developed,that allows the user to view the provided structure even before it is evaluated, that is, through mathematics symbolic.
#' 
#' @param sigmaThetaExpr A character with the covariance structure type or a list of expressions
#' @param numWaves An integer with the size of the square matrix to be printed.
#'
#' @return Return NULL and print in terminal the sigmaThetaExpr.
#'
#' @export
#' 
#' @examples 
#' sigmaThetaExpr_viewer("UCM", 5)


sigmaThetaExpr_viewer <- function(sigmaThetaExpr, numWaves = NULL) {
  checkSigmaThetaExprRequirements(sigmaThetaExpr, numWaves)

  fit <- simulateFitList(sigmaThetaExpr, numWaves)
  sigmaThetaExpr <- getSigmaThetaExprList(sigmaThetaExpr, fit)
  exprMatrix <- generateExprMatrix(sigmaThetaExpr)

  paramsCaption <- Reduce("paste", sigmaThetaExpr$paramsNames)
  message(knitr::kable(exprMatrix, align = "c", caption = paramsCaption))

  return(invisible(NULL))
}
