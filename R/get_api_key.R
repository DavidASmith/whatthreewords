#' Gets the value of the WTW_API_KEY environment variable
#'
#' @return
#' @export
#'
#' @examples
get_api_key <- function() {
  key <- Sys.getenv("WTW_API_KEY")
  if (identical(key, "")) {
    stop("No API key found, please supply with `api_key` argument or with WTW_API_KEY env var")
  }
  key
}
