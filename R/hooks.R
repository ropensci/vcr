#' Hooks class
#'
#' @keywords internal
#' @param hooks List of hooks
#' @param hook_type,x (character) Hook name
#' @param args Args passed when invoking a hook
#' @param plac Placement, one of "start" or "end"
#' @param fun A function
#' @param prepend Whether to prepend or add to the end of the string.
#' Default: `FALSE`
#' @details Helps define new hooks, hold hooks, and accessors to get and
#' use hooks.
#' \strong{Methods}
#'   \describe{
#'     \item{`invoke_hook(hook_type, args)`}{
#'       Invoke a hook, i.e., call a hook.
#'     }
#'     \item{`clear_hooks()`}{
#'       Remove all hooks.
#'     }
#'     \item{`define_hook(hook_type, fun, prepend = FALSE)`}{
#'       Define a hook.
#'     }
#'   }
#' \strong{Private Methods}
#'   \describe{
#'     \item{`make_hook(x, plac, fun)`}{
#'       Make a hook.
#'     }
#'  }
#' @format NULL
#' @usage NULL
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

    define_hook = function(hook_type, fun, prepend = FALSE) {
      private$make_hook(hook_type, if (prepend) "start" else "end", fun)
    }
  ),

  private = list(
    make_hook = function(x, plac, fun) {
      defhk <- DefinedHooks$new()
      self$hooks[[x]] <-
        defhk$set_hook(name = x,
                       placement_method = plac,
                       fun = fun
        )
    }
  )
)

# defined hooks - xxx
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
