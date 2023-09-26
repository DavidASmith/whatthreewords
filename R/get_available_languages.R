#' Gets available languages for use in the what3words API
#'
#' @return A dataframe of available languages.
#' @export
#'
#' @examplesIf !identical(Sys.getenv("WTW_API_KEY"), "")
#' get_available_languages()
get_available_languages <- function() {

  # Get API key from environment variable
  key <- whatthreewords::get_api_key()

  # Build request
  request <- httr2::request("https://api.what3words.com/v3/") |>
    httr2::req_url_path_append("available-languages") |>
    httr2::req_url_query(key = key)  |>
    httr2::req_user_agent("whatthreewords (https://github.com/DavidASmith/whatthreewords)")

  # Make request
  response <- request |>
    httr2::req_perform()

  # Return dataframe
  content <- response |>
    httr2::resp_body_json()

  out <- content$languages |>
    lapply(data.frame) |>
    dplyr::bind_rows()
  out
}
