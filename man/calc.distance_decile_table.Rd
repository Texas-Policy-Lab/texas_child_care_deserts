% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_tract_radius.R
\name{calc.distance_decile_table}
\alias{calc.distance_decile_table}
\title{Distance decile table}
\usage{
calc.distance_decile_table(df, county_list)
}
\arguments{
\item{df.}{data.frame. Default is DF_ACF.}

\item{county_list.}{A list of TX county codes of counties of interest to 
compare. E.g county_list = c(48439) for Tarrant County; 
county_list = c("48201", "48439") to compare Harris to Tarrant County}
}
\value{
table
}
\description{
Distance decile table
}
