#' Keeps track of the cassette persisters in a hash-like object
#'
#' @details There's only one: file system
#' @examples
#' (aa <- persisters$new())
#' aa$name
#' aa$persisters
#' yaml_serializer <- aa$persisters$new()
#' yaml_serializer
persisters <- R6::R6Class("persisters",
   public = list(
     persisters = list(),
     name = "FileSystem",
     initialize = function(persisters = list(), name = "FileSystem") {
       self$persisters <- persisters
       self$name <- name
       private$persister_get()
     }
   ),
   private = list(
     # Gets and sets a named persister
     persister_get = function() {
       if (!self$name %in% 'yaml') {
         stop(sprintf("The requested VCR cassette persister (%s) is not registered.", self$name),
              call. = FALSE)
       }
       self$persisters <- switch(self$name,
                                 FileSystem = FileSystem
       )
     }
   )
)
