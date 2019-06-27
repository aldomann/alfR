#' Get Git commit history
#'
#' Get your Git commit history as a data frame or raw text between two specified dates
#'
#' @param from Starting date
#' @param to Ending date
#' @param raw Whether the output is a raw string vector (TRUE), or a data frame (FALSE)
#' @keywords git
#' @export
#' @examples
#' get_git_commit_history("2019-06-01")
#'
#' @import dplyr
#' @import tidyr

get_git_commit_history <- function(from, to = Sys.Date(), raw = FALSE) {
  git_commits <- system(
    paste(
      "git log",
          "--since", lubridate::as_date(from) - 1,
          "--until", to,
          "--reverse",
          "--pretty='%Cgreen %cI ## %Creset %s'"
    ),
    intern = TRUE
  )

  if (!raw) {
    git_commits <- git_commits %>%
      data.frame() %>%
      `colnames<-`("lines") %>%
      tidyr::separate(lines, c("date", "message"), sep = "##") %>%
      dplyr::mutate(
        date = lubridate::ymd_hms(date)
      )
  }

  return(git_commits)
}
