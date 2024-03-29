% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_03_enrollment.R
\name{calc.capacity_adjustment_03}
\alias{calc.capacity_adjustment_03}
\title{Calculate 0-3 capacity estimate}
\usage{
calc.capacity_adjustment_03(df_hhsc_ccl, df_frontline, grouping_vars = NULL)
}
\arguments{
\item{df_hhsc_ccl}{data.frame.}

\item{grouping_vars}{string. The results to be grouped by. Default is NULL.}

\item{config}{list. Vector of county FIPS codes with names attributes}

\item{tract_radius}{numeric. A number indicating the tract radius in miles.}

\item{xwalk_tracts}{data.frame.}

\item{adj_tracts}{data.frame.}

\item{df_acf}{data.frame.}

\item{qtrs}{vector of strings. Default is c("1","2","4").}
}
\description{
Calculate 0-3 capacity estimate
}
