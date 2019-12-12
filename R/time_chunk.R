#' Time chunk
#'
#' This function is used to control execution time when running long simulations. Only GNU/Linux with libnotify macOS are supported right now.
#'
#' The function does the following things:
#'
#' \itemize{
#'   \item execute any R call
#'   \item print the elapsed time to execute it
#'   \item send a permanent desktop notification
#'   \item emit a completion alert sound
#' }
#' @param call Any (?) R call
#' @keywords system.time
#' @export
#' @examples
#' time_chunk(print("hello world"))
#' time_chunk(a <- seq(1:10000))
time_chunk <- function(call) {
  # R console commands
  print(paste("Started computation at", Sys.time()))
  print(system.time(call))
  print(paste("Finished computation at", Sys.time()))

  # Desktop/system commands
  if (Sys.info()["sysname"] == "Linux") {
    system("notify-send 'Finished calculations' 'Get back to work!' -i rstudio -u critical")
    system("paplay /usr/share/sounds/freedesktop/stereo/complete.oga")
  } else if (Sys.info()["sysname"] == "Darwin") {
    system(paste("osascript -e", "'", 'display notification "Get back to work!" with title "RStudio" subtitle "Finished calculations" sound name "Hero"', "'"))
  }
}
