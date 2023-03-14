#' Fit fixed and random effects longitudinal model.
#'
#' Estimate the fixed effects of the model, also known as β parameters of the regression,taking into account the sampling plan of the research, and also estimating the covariance matrix of the model considering the estimates of β
#'
#' @param formula A formula
#' @param waves a dataframe column or an array
#' @param ids a dataframe column or an array
#' @param weights a dataframe column or an array
#' @param stratum a dataframe column or an array
#' @param cluster a dataframe column or an array
#' @param data A dataframe or tibble
#' @param sigma A character or a square matrix
#'
#' @return The fit model with class 'mmcsd'.
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


mmcsd <- function(formula, waves, ids, weights, stratum, cluster, data, sigma = "identity") {
  call <- match.call(expand.dots = FALSE) # Capture function call
  modelFrame <- transformCallToFrame(call)

  modelComponents <- getModelComponentsFromModelFrame(modelFrame)
  mmcsdCheckRequirements(modelComponents, sigma)

  sigma <- getSigmaMatrix(sigma, modelComponents)

  wideModelComponents <- setLongModelComponentsToWide(modelComponents)
  individuals <- setWideModelComponentsToList(wideModelComponents)

  fit <- fitModel(individuals, sigma)
  fit <- organizeResults(fit, call, formula, wideModelComponents)

  return(fit)
}
