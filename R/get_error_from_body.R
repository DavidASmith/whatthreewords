#' Returns the error message from the response
#'
#' @param resp An httr2 response.
#'
#' @return The error code and message as a single item character vector.
#' @export
#'
#' @examples
get_error_from_body <- function(resp) {
  resp <- httr2::resp_body_json(resp)
  msg <- paste0(resp$error$code,
                ": ",
                resp$error$message)
  msg
}
