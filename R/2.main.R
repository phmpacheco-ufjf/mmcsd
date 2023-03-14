#' Fit covariance structered longitudinal model.
#'
#' Responsible for performing the modeling of the model's covariance matrix through the use of covariance structures.
#'
#' @param fit A fit model with class 'mmcsd'
#' @param fittingType A character with the fitting function type. See optins above
#' @param sigmaThetaExpr A character with the covariance structure type or a list of expressions
#' @param optimParams A list with configuration for optim function. 'Par' is required.
#'
#' @return The fit model with class 'mmcsd.theta'.
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

cov_mmcsd <- function(fit, fittingType, sigmaThetaExpr, optimParams) {
  call <- match.call() # Capture function call
  covMmcsdCheckRequirements(fit, fittingType, sigmaThetaExpr, optimParams)

  sigmaThetaExprList <- getSigmaThetaExprList(sigmaThetaExpr, fit)
  fittingFunction <- getFittingFunction(fittingType, sigmaThetaExprList, fit)

  derivativesExprList <- getDerivativesExprList(sigmaThetaExprList)
  gradientFunction <- getGradientFunction(derivativesExprList, sigmaThetaExprList, fit, fittingType)

  optimParams <- addConfigToOptimParams(optimParams, fittingFunction, gradientFunction, sigmaThetaExprList)

  fitTheta <- fitThetaModel(optimParams, sigmaThetaExprList, derivativesExprList, fittingType, fit)
  gofMeasures <- getGoodnessOfFit(fitTheta, fit, fittingType)

  fitTheta <- organizeFitTheta(fitTheta, gofMeasures, call, sigmaThetaExpr)

  return(fitTheta)
}
