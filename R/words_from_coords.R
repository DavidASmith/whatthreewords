#' Get what3words location from coordinates
#'
#' @param lat
#' @param lon
#' @param language
#' @param full_details
#'
#' @return
#' @export
#'
#' @examples
words_from_coords <- function(lat,
                              lon,
                              language = "en",
                              full_details = FALSE) {

  # Get API key from environment variable
  key <- whatthreewords::get_api_key()

  # Build request
  req <- httr2::request("https://api.what3words.com/v3/") |>
    httr2::req_url_path_append("convert-to-3wa") |>
    httr2::req_url_query(coordinates = paste0(lat,",",lon),
                  language = language,
                  key = key)

  # Make request
  resp <-  req |>
    httr2::req_perform()

  # Get JSON content of response
  body <- resp |>
    httr2::resp_body_json()

  # Return words or full details
  if(full_details) {
    out <- body
  } else {
    out <- body$words
  }
  out
}

