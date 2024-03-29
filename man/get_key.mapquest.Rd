% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_helper_functions.R
\name{get_key.mapquest}
\alias{get_key.mapquest}
\title{Checks to make sure the mapquest API key is set as an environment 
variable}
\usage{
\method{get_key}{mapquest}(
  env_var = "MAPQUEST_API_KEY",
  title = "MAPQUEST",
  url = "https://developer.mapquest.com/",
  install = "texascc::api_key.mapquest(key = 'X')"
)
}
\arguments{
\item{env_var.}{String. The environment variable to check for.}
}
\value{
TRUE if a census api key exists, error if it does not exist
}
\description{
Checks to make sure the mapquest API key is set as an environment 
variable
}
\examples{
\dontrun{
texascc::api_key.mapquest(key = "X")
texascc::get_key.mapquest()
}
}
