organizeFitTheta <- function(fitTheta, gofMeasures, call, sigmaThetaExpr) {
  fitTheta$gofMeasures <- gofMeasures
  fitTheta$call <- call

  fitTheta$sigmaThetaType <- getSigmaThetaType(sigmaThetaExpr)

  class(fitTheta) <- "mmcsd.theta"
  return(fitTheta)
}
