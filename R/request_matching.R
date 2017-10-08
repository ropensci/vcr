#' vcr request matching
#'
#' There are a number of options, some of which are on by default, some of
#' which can be used together, and some alone.
#'
#' @section matching on method:
#' Use the **method** request matcher to match requests on the HTTP method
#' (i.e. GET, POST, PUT, DELETE, etc). You will generally want to use
#' this matcher. The **method** matcher is used (along with the **uri** matcher)
#' by default if you do not specify how requests should match.
#'
#' @section matching on uri:
#' Use the **uri** request matcher to match requests on the request URI. The
#' **uri** matcher is used (along with the **method** matcher) by default
#' if you do not specify how requests should match.
#'
#' @section matching on host:
#' Use the **host** request matcher to match requests on the request host.
#' You can use this (alone, or in combination with **path**) as an
#' alternative to **uri** so that non-deterministic portions of the URI
#' are not considered as part of the request matching.
#'
#' @section matching on path:
#' Use the **path** request matcher to match requests on the path portion
#' of the request URI. You can use this (alone, or in combination with **host**)
#' as an alternative to **uri** so that non-deterministic portions of the URI
#'
#' @section matching on query string:
#' Use the **query** request matcher to match requests on the query string
#' portion of the request URI. You can use this (alone, or in combination with
#' others) as an alternative to **uri** so that non-deterministic portions of
#' the URI are not considered as part of the request matching.
#'
#' @section matching on body:
#' Use the **body** request matcher to match requests on the request body.
#'
#' @section matching on headers:
#' Use the **headers** request matcher to match requests on the request headers.
#'
#' @name request-matching
NULL
