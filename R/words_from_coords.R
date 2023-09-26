#' Get what3words address from coordinates
#'
#' @param lat Latitude
#' @param lon Longitude
#' @param language Code for the language of the returned words.
#' @param full_details Whether to return the full details from the API, or only
#' the what3words address for the coordinates.
#'
#' @return If `full_details` is FALSE (the default), returns a character vector
#' of the what3words for the submitted coordinates. Otherwise returns a list of
#' the full details returned by the API.
#' @export
#'
#' @examplesIf !identical(Sys.getenv("WTW_API_KEY"), "")
#' words_from_coords(lat = 51.5095, lon = -0.1266)
words_from_coords <- function(lat,
                              lon,
                              language = "en",
                              full_details = FALSE) {


  # Get API key from environment variable
  key <- whatthreewords::get_api_key()

  # Build requests - vectorised over coordinates
  requests <- mapply(function(lat, lon, language, key)
    httr2::request("https://api.what3words.com/v3/") |>
      httr2::req_url_path_append("convert-to-3wa") |>
      httr2::req_url_query(coordinates = paste0(lat,",",lon),
                           language = language,
                           key = key) |>
      httr2::req_user_agent("whatthreewords (https://github.com/DavidASmith/whatthreewords)") |>
      httr2::req_error(is_error = function(resp) FALSE),
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
    out <-
      mapply(function(content, response)
      {if(response$status_code == 200) {
        content$words
      } else {
        msg <- paste0("ERROR: ", content$error$code, ": ", content$error$message)
        warning(msg, call. = FALSE)
        msg
      }
      },
      content = contents,
      response = responses,
      SIMPLIFY = FALSE) |>
      unlist()
  }
  out

}

