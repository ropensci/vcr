# (x <- RequestIgnorer$new())
# x$LOCALHOST_ALIASES
# x$ignored_hosts
# x$ignore_hosts(hosts = "google.com")
# x$ignored_hosts
# x$ignore_localhost()
# x$ignored_hosts
# x$ignore_localhost_value('127.0.0.1')
# x$ignored_hosts

#' @title Request ignorer
#' @description request ignorer methods
#' @keywords internal
RequestIgnorer <- R6::R6Class(
  "RequestIgnorer",
  public = list(
    #' @field LOCALHOST_ALIASES A constant with values: 'localhost', '127.0.0.1',
    #' and '0.0.0.0'
    LOCALHOST_ALIASES = c('localhost', '127.0.0.1', '0.0.0.0'),
    #' @field ignored_hosts vector of ignored host URI's
    ignored_hosts = list(),

    #' @description Create a new `RequestIgnorer` object
    #' @return A new `RequestIgnorer` object
    initialize = function() {
      self$ignored_hosts <- EnvHash$new()
    },

    #' @description ignore all localhost values (localhost, 127.0.0.1, 0.0.0.0)
    #' @return no return; sets to ignore all localhost aliases
    ignore_localhost = function() {
      self$ignored_hosts$merge(self$LOCALHOST_ALIASES)
    },

    #' @description ignore a specific named localhost
    #' @param value (character) A localhost value to ignore, e.g., 'localhost'
    #' @return no return; defines request ignorer hook
    ignore_localhost_value = function(value) {
      self$ignore_hosts(value)
    },

    #' @description ignore any named host
    #' @param hosts (character) vector of hosts to ignore
    #' @return no return; adds host to ignore
    ignore_hosts = function(hosts) {
      # self$ignored_hosts <- c(self$ignored_hosts, hosts)
      self$ignored_hosts$merge(hosts)
    },

    #' @description method to determine whether to ignore a request
    #' @param request request to ignore
    #' @return no return; defines request ignorer hook
    should_be_ignored = function(request) {
      if (is.null(self$ignored_hosts$bucket)) return(FALSE)

      host <- parseurl(request$uri)$domain %||% NULL
      if (is.null(host)) return(FALSE)
      # update ignored hosts if any found in configuration
      if (!is.null(vcr_c$ignore_hosts)) self$ignore_hosts(vcr_c$ignore_hosts)
      if (vcr_c$ignore_localhost) self$ignore_localhost()

      self$ignored_hosts$includes(host)
    }
  )
)

# @param fun A function, of the form: coming...
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
