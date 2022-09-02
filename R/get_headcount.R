#' Get Headcount
#'
#' @param .data Existing data. Not required if piping (`%>%`) using the tidyverse workflow. 
#' @param date_val Date for the time that headcount should be calculated. Date should be in ymd format. It is recommended that all date field are correctly declared using lubridate::ymd().
#'
#' @return A numeric vector.
#' @export
#'
#' @examples
#' 
get_headcount <- function(.data, date_val){
  date_val <- as.Date(date_val)
  .data %>% 
    mutate(status = case_when(hire_date <= {{ date_val }} ~ 1,
                              TRUE ~ NA_real_),
           status = case_when(termination_date > {{ date_val }} | 
                                is.na(termination_date) ~ status,
                              termination_date < {{ date_val }} ~ 0)) %>% 
    # group_by(status, .add=TRUE) %>%
    # count() %>% 
    # filter(status == "active") %>% 
    # ungroup() %>% 
    # select(- status)
    summarize(headcount = sum(status, na.rm=TRUE))
  
  # "0" = Terminated PRIOR to the supplied date.
  # "1" = Active as of the supplied date.
  # NA = Either no term date, Term date ON the supplied date, or a term date in the future.
}