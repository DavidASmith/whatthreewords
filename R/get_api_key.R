#' Gets the value of the WTW_API_KEY environment variable
#'
#' @return The value of the `WTW_API_KEY` environment variable.
#' @export
#'
#' @examples
#' get_api_key()
get_api_key <- function() {
  key <- Sys.getenv("WTW_API_KEY")
  if (identical(key, "")) {
    stop("No API key found, please set the WTW_API_KEY environment variable.")
  }
  key
}
