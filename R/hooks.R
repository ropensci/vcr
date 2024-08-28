# (x <- Hooks$new())
# x$hooks
# x$define_hook(hook_type = "foo", fun = function(x) x ^ 2)
# x$hooks$foo(4)
# x$clear_hooks()
# x$hooks

#' @title Hooks class
#' 
#' @description Helps define new hooks, hold hooks, and accessors to get and
#' use hooks.
#'
#' @keywords internal
#' @details
#' \strong{Private Methods}
#'   \describe{
#'     \item{`make_hook(x, plac, fun)`}{
#'       Make a hook.
#'       - x (character) Hook name
#'       - plac Placement, one of "start" or "end"
#'       - fun a function/callback
#'     }
#'  }
#' @format NULL
#' @usage NULL
Hooks <- R6::R6Class(
  'Hooks',
  public = list(
    #' @field hooks intenal use
    hooks = list(),

    #' @description invoke a hook
    #' @param hook_type (character) Hook name
    #' @param args (named list) Args passed when invoking a hook
    #' @return executes hook
    invoke_hook = function(hook_type, args) {
      self$hooks[[hook_type]](args)
    },

    #' @description clear all hooks
    #' @return no return
    clear_hooks = function() {
      # clear hooks, set back to an empty list
      self$hooks <- list()
    },

    #' @description define a hook
    #' @param hook_type (character) Hook name
    #' @param fun A function
    #' @param prepend (logical) Whether to prepend or add to the end
    #' of the string. Default: `FALSE`
    #' @return no return; defines hook internally
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
