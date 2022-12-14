% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_terms.R
\name{get_terms}
\alias{get_terms}
\title{Get Terms}
\usage{
get_terms(.data, d1, month_offset = 3, term_cat = "Voluntary")
}
\arguments{
\item{.data}{Existing data. Not required if piping (\verb{\%>\%}) using the tidyverse workflow.}

\item{d1}{Date for the time that headcount should be calculated. Date should be in ymd format. It is recommended that all date field are correctly declared using lubridate::ymd().}

\item{month_offset}{Number of \emph{months} to offset the original date. This is set to 3 months by default (the span of 1 quarter.)}

\item{term_cat}{Category of terminations. Defaults to NULL, and will return all terms regardless of type. To choose a specific term category, provide a vector of unique words within the categories required (selection uses \code{grepl()}).}
}
\value{
A tibble with number of termed within the time frame.
}
\description{
Get Terms
}
\examples{
#' # Standard usage.
wfm_tms_play \%>\% 
  get_terms("2019-01-01")
  
# Standard usage -- "voluntary" terms only.
wfm_tms_play \%>\% 
  count(termination_category)
  
wfm_tms_play \%>\% 
  get_terms("2019-01-01", term_cat = c("Voluntary"))
  
# Using the group_by() dplyr function.  
wfm_tms_play \%>\% 
  dplyr::group_by(region) \%>\% 
  get_terms("2019-01-01")
}
