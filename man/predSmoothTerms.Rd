% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predSmoothTerms.R
\name{predSmoothTerms}
\alias{predSmoothTerms}
\title{Predict values for a smooth term}
\usage{
predSmoothTerms(model, grids)
}
\arguments{
\item{model}{- [mgcv::gam()] model object}

\item{grids}{- named list of 1D covariate grids (names should match model formula covariates)}
}
\value{
named list with a tibble dataframe (tbl_df object) for each smooth term as elements.
Names are the corresponding smooth term name.
}
\description{
Function to predict values for a smooth term
}
