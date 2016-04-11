#' Request ignorer
#'
#' @keywords internal
#' @details Hook to handle request ignorers, including:
#' \itemize{
#'  \item ignore_request -  will ignore any request for which the given
#'  function returns \code{TRUE}
#'  \item ignore_hosts - ignore any named host
#'  \item ignore_localhost - ignore all localhost values (localhost, 127.0.0.1, 0.0.0.0)
#'  \item ignore_localhost_value - ignore a specific named localhost
#' }
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
