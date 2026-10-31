local_light_switch <- function(frame = parent.frame()) {
  old <- the$light_switch
  defer(the$light_switch <- old, frame)
}

desc_text <- "Package: %s
Title: Does A Thing
Description: Does a thing.
Version: 0.0.1
Author: Jane Doe
Maintainer: Jane Doe <jane@doe.com>
License: MIT + file LICENSE
LazyData: true
RoxygenNote: 6.1.1
Suggests:
    testthat\n"

make_pkg <- function(frame = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = frame)

  dir_create(file.path(dir, "man"))
  dir_create(file.path(dir, "R"))
  dir_create(file.path(dir, "tests", "testthat"))
  cat(sprintf(desc_text, basename(dir)), file = file.path(dir, "DESCRIPTION"))

  dir
}

has_port <- function(port) crul::ok(paste0('http://localhost:', port))

skip_if_localhost_8000_gone <- function() {
  if (has_port(8000)) {
    return()
  }
  testthat::skip("port 8000 not available")
}

check_url <- function(x, ...) {
  suppressWarnings(suppressMessages(crul::ok(x, ...)))
}

hb <- function(x = NULL) {
  server <- getOption("vcr::httpbin_local_server")
  if (is.null(server)) {
    app <- webfakes::httpbin_app()
    server <- webfakes::new_app_process(app)
    options(`vcr::httpbin_local_server` = server)
  }

  server$url(x)
}

hb_remote <- function(x = NULL) {
  skip_on_cran()

  base_url <- getOption("vcr::httpbin_server")
  if (is.null(base_url)) {
    base_url <- find_httpbin_server()
    options(`vcr::httpbin_server` = base_url)
  }

  if (is.null(x)) {
    base_url
  } else {
    paste0(base_url, x)
  }
}

find_httpbin_server <- function() {
  urls <- c(
    "https://hb.cran.dev",
    # "https://hb.opencpu.org",
    "https://nghttp2.org/httpbin"
  )
  h <- curl::new_handle(timeout = 10, failonerror = FALSE)

  for (i in seq_along(urls)) {
    url <- urls[[i]]
    out <- curl::curl_fetch_memory(url, handle = h)
    if (out$status_code == 200) {
      cat(paste0("using base url for tests: ", url), sep = "\n")
      return(url)
    }
  }
  stop("all httpbin servers down")
}

recorded_at <- function(x) {
  read_yaml(x$file())$http_interactions[[1]]$recorded_at
}
read_cassette <- function(name) {
  read_yaml(file.path(the$config$dir, name))
}
read_yaml <- function(path) {
  if (!file.exists(path)) {
    cli::cli_abort("{.path {path}} does not exist.")
  }
  yaml::yaml.load_file(path)
}

testthat::set_state_inspector(\() {
  temp_files <- dir(tempdir())
  temp_files <- temp_files[!grepl("^callr", temp_files)]
  temp_files <- temp_files[!grepl("^webfakes", temp_files)]

  list(
    temp_files = temp_files,
    wd_files = dir(),
    vcr_config = the$config
  )
})

chat_app <- function() {
  app <- webfakes::new_app()
  app$use(webfakes::mw_log())
  app$use(webfakes::mw_json())

  app$post("/v1/chat/completions", function(req, res) {
    msgs <- req$json$messages
    last <- msgs[[length(msgs)]]$content

    # failure injection via header
    if (identical(req$get_header("X-Mock-Error"), "429")) {
      res$set_status(429)$send_json(
        list(error = list(message = "rate limited", type = "rate_limit_error")),
        auto_unbox = TRUE
      )
      return()
    }

    res$send_json(
      list(
        id = "chatcmpl-mock",
        object = "chat.completion",
        model = if (is.null(req$json$model)) "mock-model" else req$json$model,
        choices = list(list(
          index = 0L,
          message = list(role = "assistant", content = paste("echo:", last)),
          finish_reason = "stop"
        )),
        usage = list(
          prompt_tokens = 1L,
          completion_tokens = 1L,
          total_tokens = 2L
        )
      ),
      auto_unbox = TRUE
    )
  })

  # streaming (SSE) variant
  app$post("/v1/chat/completions/stream", function(req, res) {
    res$set_header("Content-Type", "text/event-stream")
    for (w in c("Hello", " from", " mock")) {
      chunk <- list(choices = list(list(delta = list(content = w), index = 0L)))
      res$send_chunk(paste0(
        "data: ",
        jsonlite::toJSON(chunk, auto_unbox = TRUE),
        "\n\n"
      ))
    }
    res$send_chunk("data: [DONE]\n\n")
  })

  app
}
