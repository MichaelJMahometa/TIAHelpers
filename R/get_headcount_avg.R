#' Get Headcount Average
#'
#' @param .data Existing data. Not required if piping (`%>%`) using the tidyverse workflow.
#' @param d1 Date for the time that headcount should be calculated. Date should be in ymd format. It is recommended that all date field are correctly declared using lubridate::ymd().
#' @param month_offset Number of *months* to offset the original date. This is set to 3 months by default (the span of 1 quarter.)
#'
#' @return Tibble containing headcounts at two dates plus the average headcount.
#' @export
#'
#' @examples
#' # Standard usage.
#' wfm_tms_play %>% 
#'   get_headcount_avg("2019-01-01")
#'   
#' # Using the group_by() dplyr function.  
#' wfm_tms_play %>% 
#'   dplyr::group_by(region) %>% 
#'   get_headcount_avg("2019-01-01")
#'   
#' # Same, but for the month of Jan.  
#' wfm_tms_play %>% 
#'   dplyr::group_by(region) %>% 
#'   get_headcount_avg("2019-01-01", month_offset = 1)
get_headcount_avg <- function(.data, d1, month_offset = 3){
  d1 <- as.Date(d1)
  d2 <- max(d1, d1 + lubridate:::months.numeric(month_offset) - 1)
  .data %>% 
    get_headcount(d1) %>% 
    dplyr::rename(headcount_date_1 = headcount) -> s1
  
  .data %>% 
    get_headcount(d2)  %>% 
    dplyr::rename(headcount_date_2 = headcount) -> s2
  
  if(length(s1) == 1){
    tibble::tibble(s1, s2) %>% 
      dplyr::rowwise() %>% 
      dplyr::mutate(average = tibble::num(mean(c(headcount_date_1, headcount_date_2), na.rm=TRUE), 
                           digits=3)) %>% # added 3 digit option
      dplyr::ungroup()
  } else {
    suppressMessages(
      s1 %>% 
        dplyr::left_join(s2) %>% 
        dplyr::rowwise() %>% 
        dplyr::mutate(average = tibble::num(mean(c(headcount_date_1, headcount_date_2), na.rm=TRUE), 
                             digits=3)) %>% # added 3 digit option
        dplyr::ungroup()
    )
  }
}
