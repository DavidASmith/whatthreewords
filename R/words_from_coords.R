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

  # Build requests
  requests <- mapply(function(lat, lon, language, key)
    httr2::request("https://api.what3words.com/v3/") |>
      httr2::req_url_path_append("convert-to-3wa") |>
      httr2::req_url_query(coordinates = paste0(lat,",",lon),
                           language = language,
                           key = key),
    lat = lat,
    lon = lon,
    MoreArgs = list(language = language,
                    key = key),
    SIMPLIFY = FALSE)


  # Make requests
  responses <-  requests |>
    lapply(httr2::req_perform)

  # Get JSON content of response
  contents <- responses |>
    lapply(httr2::resp_body_json)

  # Return words or full details
  if(full_details) {
    out <- contents
  } else {
    out <- contents |>
      lapply(function(x) x$words) |>
      unlist()
  }
  out

}

