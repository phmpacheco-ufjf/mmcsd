#' Summarise the results of 'cov_mmcsd' fit.
#'
#' @param object A mmcsd.theta fitted model
#' @param ... Additional params passed to summary
#'
#' @return Return NULL and print in terminal the results.
#'
#' @export
#' 
#' @examples
#' fit <- mmcsd(
#'  score ~ wave + ageg + ecacg + qualifg,
#'  waves = wave, ids = id,
#'  weights = weight, stratum = strata, cluster = cluster,
#'  data = example_data, sigma = "exchangeable"
#' )
#' fitTheta_ucm <- cov_mmcsd(fit,
#'  fittingType = "PML", sigmaThetaExpr = "UCM",
#'  optimParams = list(par = c(7, 5))
#' )

summary.mmcsd.theta <- function(object, ...) {
  thetaTable <- getThetaTable(object)
  optimTable <- getOptimTable(object)

  catCall(object)
  catFitTypes(object)
  catFitResults(object, thetaTable)
  catOptimDetails(object, optimTable)
  catSigmaThetaMatrix(object)
  catGoodnessOfFitMeasures(object)

  return(invisible(NULL))
}
