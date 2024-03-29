% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ccl_dm.R
\name{dm.hhsc_ccl}
\alias{dm.hhsc_ccl}
\title{HHSC CCL data management}
\usage{
\method{dm}{hhsc_ccl}(
  df,
  input_columns = list(OPERATION_NUMBER = "character", OPERATION_NAME = "character",
    OPERATION_TYPE = "character", Location_address_geo = "character", COUNTY =
    "character", TOTAL_CAPACITY = "numeric", LICENSED_TO_SERVE_AGES = "character",
    PROGRAMS_PROVIDED = "character", ACCEPTS_CHILD_CARE_SUBSIDIES = "character",
    email_address = "character", PHONE_NUMBER = "character"),
  county_fips = NULL,
  name,
  state_fips,
  df_twc,
  naeyc_pth1,
  naeyc_pth2,
  ...
)
}
\arguments{
\item{df}{data.frame. The dataframe}

\item{input_columns.}{List. List of the columns to keep.}

\item{county_fips.}{Integer. The FIPS code for the county.}

\item{pth.}{string. Path to store the processed data.}

\item{name.}{string. Name of the data.}
}
\value{
data.frame
}
\description{
Clean CCL download data, convert key variables to binary and 
select variables
}
