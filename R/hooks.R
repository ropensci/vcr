#' Hooks class
#' @keywords internal
#' @details Helps define new hooks, hold hooks, and accessors to get and use hooks.
#' @examples \dontrun{
#' (x <- Hooks$new())
#' x$hooks
#' x$define_hook(hook_type = "foo", fun = function(x) x ^ 2)
#' x$hooks$foo(4)
#' x$clear_hooks()
#' x$hooks
#' }
Hooks <- R6::R6Class(
  'Hooks',
  public = list(
    hooks = list(),

    invoke_hook = function(hook_type, args) {
      self$hooks[[hook_type]](args)
    },

    clear_hooks = function() {
      # clear hooks, set back to an empty list
      self$hooks <- list()
    },

    make_hook = function(x, plac, fun) {
      defhk <- DefinedHooks$new()
      self$hooks[[x]] <-
        defhk$set_hook(name = x,
          placement_method = plac,
          fun = fun
        )
    },

    define_hook = function(hook_type, fun, prepend = FALSE) {
      self$make_hook(hook_type, if (prepend) "start" else "end", fun)
    }
  )
)

DefinedHooks <- R6::R6Class(
  'DefinedHooks',
  public = list(
    hooks = list(),

    set_hook = function(name, placement_method, fun) {
      attr(fun, "placement_method") <- placement_method
      self$hooks[[name]] <- fun
      return(self$hooks[[name]])
    }
  )
)
