% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/request_matching.R
\name{request-matching}
\alias{request-matching}
\title{vcr request matching}
\description{
There are a number of options, some of which are on by default, some of
which can be used together, and some alone.
}
\details{
To match previously recorded requests, \code{vcr} has to try to match new
HTTP requests to a previously recorded one. By default, we match on HTTP
method (e.g., \code{GET}) and URI (e.g., \verb{http://foo.com}), following Ruby’s
VCR gem.

You can customize how we match requests with one or more of the
following options, some of which are on by default, some of which can be
used together, and some alone.
\itemize{
\item \code{method}: Use the \strong{method} request matcher to match requests on the
HTTP method (i.e. GET, POST, PUT, DELETE, etc). You will generally
want to use this matcher. The \strong{method} matcher is used (along with
the \strong{uri} matcher) by default if you do not specify how requests
should match.
\item \code{uri}: Use the \strong{uri} request matcher to match requests on the
request URI. The \strong{uri} matcher is used (along with the \strong{method}
matcher) by default if you do not specify how requests should match.
\item \code{host}: Use the \strong{host} request matcher to match requests on the
request host. You can use this (alone, or in combination with
\strong{path}) as an alternative to \strong{uri} so that non-deterministic
portions of the URI are not considered as part of the request
matching.
\item \code{path}: Use the \strong{path} request matcher to match requests on the path
portion of the request URI. You can use this (alone, or in combination
with \strong{host}) as an alternative to \strong{uri} so that non-deterministic
portions of the URI
\item \code{query}: Use the \strong{query} request matcher to match requests on the
query string portion of the request URI. You can use this (alone, or
in combination with others) as an alternative to \strong{uri} so that
non-deterministic portions of the URI are not considered as part of
the request matching.
\item \code{body}: Use the \strong{body} request matcher to match requests on the
request body.
\item \code{headers}: Use the \strong{headers} request matcher to match requests on
the request headers.
}

You can set your own options by tweaking the \code{match_requests_on}
parameter in \code{use_cassette()}:

\if{html}{\out{<div class="sourceCode r">}}\preformatted{library(vcr)
}\if{html}{\out{</div>}}

\if{html}{\out{<div class="sourceCode r">}}\preformatted{use_cassette(name = "foo_bar", \{
    cli$post("post", body = list(a = 5))
  \}, 
  match_requests_on = c('method', 'headers', 'body')
)
}\if{html}{\out{</div>}}
\subsection{Matching}{
\subsection{headers}{

\if{html}{\out{<div class="sourceCode r">}}\preformatted{library(crul)
library(vcr)
cli <- crul::HttpClient$new("https://httpbin.org/get", 
  headers = list(foo = "bar"))
use_cassette(name = "nothing_new", \{
    one <- cli$get()
  \}, 
  match_requests_on = 'headers'
)
cli$headers$foo <- "stuff"
use_cassette(name = "nothing_new", \{
    two <- cli$get()
  \}, 
  match_requests_on = 'headers'
)
one$request_headers
two$request_headers
}\if{html}{\out{</div>}}
}

}
}
