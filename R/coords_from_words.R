#' Returns the coordinates for a given what3words address
#'
#' @param words
#' @param full_details
#'
#' @return
#' @export
#'
#' @examples
coords_from_words <- function(words,
                              full_details) {

  # Get API key from environment variable
  key <- whatthreewords::get_api_key()

  # Build requests - vectorised over coordinates
  requests <- mapply(function(words, key)
    httr2::request("https://api.what3words.com/v3/") |>
      httr2::req_url_path_append("convert-to-coordinates") |>
      httr2::req_url_query(words = words,
                           key = key),
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
    out <- contents |>
      lapply(function(x) matrix(c(x$coordinates$lat, x$coordinates$lng), nrow = 1))
    out <- do.call(rbind, out)
    colnames(out) <- c("lat", "lon")
  }
  out
}
