test_that("API returns words from coordinates (single)", {
  skip_if(identical(Sys.getenv("WTW_API_KEY"), ""),
          message = "API key environment variable not set.")
  expect_identical(words_from_coords(lat = 51.5095, lon = -0.1266),
                   "lamp.inner.dent")
})

test_that("API returns words from coordinates (vectorised)", {
  skip_if(identical(Sys.getenv("WTW_API_KEY"), ""),
          message = "API key environment variable not set.")
  expect_identical(words_from_coords(lat = c(53.3703, 53.41145, 53.3096),
                                     lon = c(-1.47119, -1.500204, -1.478715)),
                   c("elite.icon.levels", "weep.stands.shack", "soon.belt.owls")
  )
})

test_that("Full details are returned (single)", {
  skip_if(identical(Sys.getenv("WTW_API_KEY"), ""),
          message = "API key environment variable not set.")
  full_details <- words_from_coords(lat = 51.5095,
                                    lon = -0.1266,
                                    full_details = TRUE)
  expect_type(full_details, "list")
  expect_identical(full_details$words, "lamp.inner.dent")
})


test_that("Full details are returned (vectorised)", {
  skip_if(identical(Sys.getenv("WTW_API_KEY"), ""),
          message = "API key environment variable not set.")
  full_details <- words_from_coords(lat = c(53.3703, 53.41145, 53.3096),
                                    lon = c(-1.47119, -1.500204, -1.478715),
                                    full_details = TRUE)
  expect_type(full_details, "list")
  expect_equal(length(full_details), 3)
  expect_identical(full_details[[1]]$words, "elite.icon.levels")
})

test_that("Errors are handled", {
  skip_if(identical(Sys.getenv("WTW_API_KEY"), ""),
          message = "API key environment variable not set.")
  invalid_coord_example <- suppressWarnings(words_from_coords(lat = 100, lon = -0.1266))
  expect_match(invalid_coord_example, "ERROR")
})
