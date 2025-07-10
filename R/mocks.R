vcr_httr_mock <- function(on) {
  if (!is_installed("httr")) {
    return()
  }

  if (on) {
    httr::set_callback("request", \(req) RequestHandlerHttr$new(req)$handle())
  } else {
    httr::set_callback("request", NULL)
  }
}

vcr_httr2_mock <- function(on) {
  if (!is_installed("httr2")) {
    return()
  }

  if (on) {
    options(httr2_mock = \(req) RequestHandlerHttr2$new(req)$handle())
  } else {
    options(httr2_mock = NULL)
  }
}

vcr_crul_mock <- function(on) {
  if (!is_installed("crul")) {
    return()
  }

  if (on) {
    options(crul_mock = \(req) RequestHandlerCrul$new(req)$handle())
  } else {
    options(crul_mock = NULL)
  }
}

enable_mocks <- function() {
  vcr_httr_mock(on = TRUE)
  vcr_httr2_mock(on = TRUE)
  vcr_crul_mock(on = TRUE)
}

disable_mocks <- function() {
  vcr_httr_mock(on = FALSE)
  vcr_httr2_mock(on = FALSE)
  vcr_crul_mock(on = FALSE)
}
