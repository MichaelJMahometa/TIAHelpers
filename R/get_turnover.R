#' Get Turnover
#'
#' @param .data Existing data. Not required if piping (`%>%`) using the tidyverse workflow.
#' @param d1 Date for the time that headcount should be calculated. Date should be in ymd format. It is recommended that all date field are correctly declared using lubridate::ymd().
#' @param month_offset Number of *months* to offset the original date. This is set to 3 months by default (the span of 1 quarter.)
#' @param term_cat Category of terminations. Defaults to "Voluntary". 
#'
#' @return A tibble with all infomation that makes up turnover rate for the time period requested.
#' @export
#'
#' @examples
#' # Standard usage.
#' wfm_tms_play %>% 
#'   get_turnover("2019-01-01")
#'   
#' # Standard usage -- both types of term category.
#' wfm_tms_play %>% 
#'   get_turnover("2019-01-01", term_cat = c("Voluntary", "Involuntary"))
#'   
#' # Using the group_by() dplyr function.  
#' wfm_tms_play %>% 
#'   dplyr::group_by(region) %>% 
#'   get_turnover("2019-01-01")
get_turnover <- function(.data, d1, month_offset = 3, term_cat = "Voluntary"){
  d1 <- as.Date(d1)
  .data %>% 
    get_headcount_avg(d1, month_offset = month_offset) -> t1
  
  .data %>% 
    get_terms(d1, month_offset = month_offset, term_cat = term_cat) -> t2
  
  if(length(t2) == 1){
    tibble::tibble(t1, t2) %>% 
      dplyr::mutate(turnover_rate = (termed / average) * 100)
  } else {
    suppressMessages(
      t1 %>% 
        dplyr::left_join(t2) %>% 
        dplyr::mutate(turnover_rate = (termed / average) * 100)
    )
  }
}