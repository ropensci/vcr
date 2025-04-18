RequestIgnorer <- R6::R6Class(
  "RequestIgnorer",
  public = list(
    ignored_hosts = character(),
    ignored_localhost = FALSE,

    ignore_localhost = function() {
      self$ignored_localhost <- TRUE
    },

    ignore_hosts = function(hosts) {
      self$ignored_hosts <- union(self$ignored_hosts, hosts)
    },

    currently_ignored_hosts = function() {
      ignore_localhost <- self$ignored_localhost ||
        isTRUE(vcr_c$ignore_localhost)
      c(
        self$ignored_hosts,
        vcr_c$ignore_hosts,
        if (ignore_localhost) LOCALHOST_ALIASES
      )
    },

    should_be_ignored = function(request) {
      host <- parseurl(request$uri)$domain
      host %in% self$currently_ignored_hosts()
    }
  )
)

LOCALHOST_ALIASES <- c('localhost', '127.0.0.1', '0.0.0.0')
