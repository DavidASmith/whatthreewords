#' Returns the coordinates for a given what3words address
#'
#' @param words A what3words location
#' @param full_details `FALSE` (default) to return only the coordinates.
#' `TRUE` for full details.
#'
#' @return If `full_details == FALSE` (the default) a matrix of WGS84
#' coordinates. Otherwise a list of full details for each what3words location.
#' @export
#'
#' @examplesIf !identical(Sys.getenv("WTW_API_KEY"), "")
#' coords_from_words("hotdog.jumping.frog")
coords_from_words <- function(words,
                              full_details = FALSE) {

  # Get API key from environment variable
  key <- whatthreewords::get_api_key()

  # Build requests - vectorised over coordinates
  requests <- mapply(function(words, key)
    httr2::request("https://api.what3words.com/v3/") |>
      httr2::req_url_path_append("convert-to-coordinates") |>
      httr2::req_url_query(words = words,
                           key = key)  |>
      httr2::req_user_agent("whatthreewords (https://github.com/DavidASmith/whatthreewords)") |>
      httr2::req_error(is_error = function(resp) FALSE),
    words = words,
    MoreArgs = list(key = key),
    SIMPLIFY = FALSE)


  # Make requests
  responses <-  requests |>
    lapply(httr2::req_perform)

  # Get JSON content of response
  contents <- responses |>
    lapply(httr2::resp_body_json)

  # Return coordinates or full details
  if(full_details) {
    out <- contents
  } else {
    out <- mapply(function(content, response)
    {if(response$status_code == 200) {
      matrix(c(content$coordinates$lat, content$coordinates$lng),
             nrow = 1)
    } else {
      msg <- paste0("ERROR: ", content$error$code, ": ", content$error$message)
      warning(msg, call. = FALSE)
      matrix(c(NA, NA), nrow = 1)
    }
    },
    content = contents,
    response = responses,
    SIMPLIFY = FALSE)

    out <- do.call(rbind, out)
    colnames(out) <- c("lat", "lon")
  }
  out
}
