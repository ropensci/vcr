compact <- function(x) Filter(Negate(is.null), x)

check_request_matchers <- function(
  x,
  error_arg = caller_arg(x),
  error_call = caller_env()
) {
  if (is.null(x)) {
    return()
  }
  if (identical(x, "default")) {
    return()
  }
  vals <- c(
    "method",
    "uri",
    "headers",
    "host",
    "path",
    "body",
    "body_json",
    "query"
  )
  arg_match(
    x,
    vals,
    error_arg = error_arg,
    error_call = error_call,
    multiple = TRUE
  )
}

check_record_mode <- function(
  x,
  error_arg = caller_arg(x),
  error_call = caller_env()
) {
  if (is.null(x)) {
    return()
  }
  vals <- c("none", "once", "new_episodes", "all")
  arg_match(x, vals, error_arg = error_arg, error_call = error_call)
}

check_list <- function(
  x,
  allow_null = FALSE,
  error_arg = caller_arg(x),
  error_call = caller_env()
) {
  if (is.list(x)) {
    return()
  }
  if (is.null(x) && allow_null) {
    return()
  }

  stop_input_type(x, "a list", arg = error_arg, call = error_call)
}

dir_create <- function(path) {
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  path
}

cur_time <- function(tz = "") {
  format_time(Sys.time(), tz = tz)
}

format_time <- function(x, tz = "UTC") {
  format(x, format = "%Y-%m-%d %H:%M:%S", tz = tz)
}


pkg_versions <- function() {
  paste(
    paste0("vcr/", utils::packageVersion("vcr")),
    sep = ", "
  )
}

# for mocking
Sys.time <- NULL

cat_line <- function(...) {
  cat(paste0(..., "\n", collapse = ""))
}

set_env_var <- function(values) {
  check_list(values)
  old <- as.list(Sys.getenv(names(values), unset = NA, names = TRUE))

  for (nm in names(values)) {
    val <- values[[nm]]
    if (is.na(val)) {
      Sys.unsetenv(nm)
    } else {
      exec(Sys.setenv, !!!set_names(list(val), nm))
    }
  }

  invisible(old)
}

# modified from purrr:::has_names
along_rep <- function(x, y) rep(y, length.out = length(x))
hz_namez <- function(x) {
  nms <- names(x)
  if (is_null(nms)) {
    along_rep(x, FALSE)
  } else {
    !(is.na(nms) | nms == "")
  }
}

defer <- function(expr, env = caller_env(), after = FALSE) {
  thunk <- as.call(list(function() expr))
  do.call(on.exit, list(thunk, TRUE, after), envir = env)
}
