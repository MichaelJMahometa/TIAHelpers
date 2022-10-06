#' Get Terms
#'
#' @param .data Existing data. Not required if piping (`%>%`) using the tidyverse workflow.
#' @param d1 Date for the time that headcount should be calculated. Date should be in ymd format. It is recommended that all date field are correctly declared using lubridate::ymd().
#' @param month_offset Number of *months* to offset the original date. This is set to 3 months by default (the span of 1 quarter.)
#' @param term_cat Category of terminations. Defaults to NULL, and will return all terms regardless of type. To choose a specific term category, provide a vector of unique words within the categories required (selection uses `grepl()`). 
#'
#' @return A tibble with number of termed within the time frame.
#' @export
#'
#' @examples
#' #' # Standard usage.
#' wfm_tms_play %>% 
#'   get_terms("2019-01-01")
#'   
#' # Standard usage -- "voluntary" terms only.
#' wfm_tms_play %>% 
#'   count(termination_category)
#'   
#' wfm_tms_play %>% 
#'   get_terms("2019-01-01", term_cat = c("Voluntary"))
#'   
#' # Using the group_by() dplyr function.  
#' wfm_tms_play %>% 
#'   dplyr::group_by(region) %>% 
#'   get_terms("2019-01-01")
get_terms <- function(.data, d1, month_offset = 3, term_cat = NULL){
  # NOTE: This counts the folks termed between OR INCLUDING 2 dates
  d1 <- as.Date(d1)
  d2 <- max(d1, d1 + lubridate:::months.numeric(month_offset) - 1)
  if(is.null(term_cat)){
    .data %>% 
      dplyr::mutate(termed = dplyr::case_when(((termination_date >= d1) & (termination_date <= d2)) &
                                                !is.na(termination_category) ~ 1)) %>% 
      dplyr::summarise(termed = sum(termed, na.rm=TRUE))  
  } else {
    .data %>% 
      dplyr::mutate(termed = dplyr::case_when(((termination_date >= d1) & (termination_date <= d2)) &
                                  grepl(paste(term_cat, collapse = "|"), termination_category) ~ 1)) %>% 
      dplyr::summarise(termed = sum(termed, na.rm=TRUE))
  }
}
