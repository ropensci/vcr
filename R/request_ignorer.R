#' Request ignorer
#'
#' @keywords internal
#' @param LOCALHOST_ALIASES A constant with values: 'localhost', '127.0.0.1', and '0.0.0.0'
#' @param ignored_hosts Vector of ignored hosts
#' @param fun A function, of the form: coming...
#' @param value A localhost value to ignore, e.g, 'localhost'
#' @param hosts Character vector of hosts to ignore
#' @param request A request
#' @details Hook to handle request ignorers, including:
#'
#' \strong{Methods}
#'   \describe{
#'     \item{\code{ignore_request(fun)}}{
#'       Will ignore any request for which the given function returns \code{TRUE}
#'     }
#'     \item{\code{ignore_localhost()}}{
#'       ignore all localhost values (localhost, 127.0.0.1, 0.0.0.0)
#'     }
#'     \item{\code{ignore_localhost_value(value)}}{
#'       ignore a specific named localhost
#'     }
#'     \item{\code{ignore_hosts(hosts)}}{
#'       ignore any named host
#'     }
#'     \item{\code{ignore(request)}}{
#'
#'     }
#'   }
#' \strong{Private Methods}
#'  \describe{
#'     \item{\code{ignored_hosts_init()}}{
#'       Initialize an empty ignored hosts object on package load
#'     }
#'   }
#' @format NULL
#' @usage NULL
#' @examples \dontrun{
#' (x <- RequestIgnorer$new())
#' x$LOCALHOST_ALIASES
#' x$ignored_hosts
#' x$ignore_hosts(hosts = "google.com")
#' x$ignored_hosts
#' x$ignore_localhost()
#' x$ignored_hosts
#' x$ignore_localhost_value('127.0.0.1')
#' x$ignored_hosts
#' }

RequestIgnorer <- R6::R6Class(
  "RequestIgnorer",
  public = list(
    LOCALHOST_ALIASES = c('localhost', '127.0.0.1', '0.0.0.0'),
    ignored_hosts = list(),

    initialize = function() {
      private$ignored_hosts_init()
    },

    ignore_request = function(fun) {
      VCRHooks$define_hook(hook_type = "ignore_request", fun = fun)
      # host <- request$parsed_uri$host
      # host %in% self$ignored_hosts
    },

    ignore_localhost = function() {
      #self$ignored_hosts$reject(function(x) x %in% self$LOCALHOST_ALIASES)
      self$ignored_hosts$merge(self$LOCALHOST_ALIASES)
    },

    ignore_localhost_value = function(value) {
      self$ignore_hosts(value)
    },

    ignore_hosts = function(hosts) {
      self$ignored_hosts$merge(hosts)
    },

    ignore = function(request) {
      VCRHooks$invoke_hook(hook_type = "ignore_request", args = request)
    }
  ),

  private = list(
    ignored_hosts_init = function() {
      self$ignored_hosts <- EnvHash$new()
    }
  )
)

EnvHash <- R6::R6Class(
  "EnvHash",
  public = list(
    bucket = c(),

    print = function(...) {
      bucks <- self$bucket
      if (length(bucks) == 0) "" else bucks
      cat("<ignored hosts>", sep = "\n")
      cat(paste0("  ", paste0(bucks, collapse = ", ")), sep = "\n")
      invisible(self)
    },

    merge = function(vals) {
      self$bucket <- unique(c(self$bucket, vals))
    },

    includes = function(name) {
      name %in% self$bucket
    },

    reject = function(fun) {
      self$bucket <- Filter(Negate(fun), self$bucket)
    }
  )
)
