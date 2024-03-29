% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/1.main.R
\name{mmcsd}
\alias{mmcsd}
\title{Fit fixed and random effects longitudinal model.}
\usage{
mmcsd(formula, waves, ids, weights, stratum, cluster, data, sigma = "identity")
}
\arguments{
\item{formula}{A formula}

\item{waves}{a dataframe column or an array}

\item{ids}{a dataframe column or an array}

\item{weights}{a dataframe column or an array}

\item{stratum}{a dataframe column or an array}

\item{cluster}{a dataframe column or an array}

\item{data}{A dataframe or tibble}

\item{sigma}{A character or a square matrix}
}
\value{
The fit model with class 'mmcsd'.
}
\description{
Fit fixed and random effects longitudinal model.
}
