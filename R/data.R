#' Dummy dataset mimicking all_tms
#'
#' A dataset hire date, termination date, and the like. Fabricated.
#'
#' @format A data frame with 50000 rows and 11 variables:
#' \describe{
#'   \item{tm_name}{Fictitious employee ID.}
#'   \item{region}{Fictitious region of employment.}
#'   \item{team}{Fictitious team assignment.}
#'   \item{hire_date}{Fictitious hire date in "YYYY-MM-DD" format (lubridate::ymd).}
#'   \item{termination_date}{Fictitious termination date.}
#'   \item{termination_category}{Fictitious termination category. Two categorical entries.}
#'   \item{termination_reason}{Fictitious Termination reason. Categorical.}
#'   \item{tenure}{Fictitious amount of time TM is employed.}
#'   \item{gender}{Fictitious gender of TM. Two cateogircal entries.}
#'   \item{is_employee_active}{Fictitious TM active status. Categorical.}
#'   \item{most_recent_job_satisfaction_score}{Fictitious Likert job satisfaction score.}
#' }
"wfm_tms_play"