% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/examples.R
\name{insert_example_cassette}
\alias{insert_example_cassette}
\title{Use cassettes in examples}
\usage{
insert_example_cassette(
  name,
  package,
  record = NULL,
  match_requests_on = NULL,
  serialize_with = NULL,
  preserve_exact_body_bytes = NULL,
  re_record_interval = NULL
)
}
\arguments{
\item{name}{The name of the cassette. This is used to name a file on
disk, so it must be valid file name.}

\item{package}{Package name.}

\item{record}{Record mode. This will be \code{"once"} if \code{package} is under
development, (i.e. loaded by devtools) and \code{"none"} otherwise. This makes
it easy to record during development and ensure that cassettes HTTP
requests are never made on CRAN.

To re-record all cassettes, you can delete \verb{inst/_vcr} then run
\code{pkgdown::build_reference(lazy = FALSE)}.}

\item{match_requests_on}{Character vector of request matchers used to
determine which recorded HTTP interaction to replay. The default matches
on the \code{"method"}, \code{"uri"}, and either \code{"body"} (if present)
or \code{"body_json"} (if the content-type is \code{application/json}).

The full set of possible values are:
\itemize{
\item \code{method}: the HTTP method.
\item \code{uri}: the complete request URI, excluding the port.
\item \code{uri_with_port}: the complete request URI, including the port.
\item \code{host}: the \strong{host} component of the URI.
\item \code{path}: the \strong{path} component of the URI.
\item \code{query}: the \strong{query} component of the URI.
\item \code{body}: the request body.
\item \code{body_json}: the request body, parsed as JSON.
\item \code{header}: all request headers.
}

If more than one is specified, all components must match in order for the
request to match. If not supplied, defaults to \code{c("method", "uri")}.

Note that the request header and body will only be included in the
cassette if  \code{match_requests_on} includes "header" or "body" respectively.
This keeps the recorded request as lightweight as possible.}

\item{serialize_with}{(string) Which serializer to use:
\code{"yaml"} (the default), \code{"json"}, or \code{"qs2"}.}

\item{preserve_exact_body_bytes}{(logical) Force a binary (base64)
representation of the request and response bodies? By default, vcr
will look at the \code{Content-Type} header to determine if this is necessary,
but if it doesn't work you can set \code{preserve_exact_body_bytes = TRUE} to
force it.}

\item{re_record_interval}{(integer) How frequently (in seconds) the
cassette should be re-recorded. Default: \code{NULL} (not re-recorded).}
}
\description{
\code{insert_example_cassette()} is a wrapper around \code{\link[=insert_cassette]{insert_cassette()}} that
stores cassettes in \verb{inst/_vcr/}. Call it in the first line of your examples
(typically wrapped in \verb{\dontshow{}}), and call \code{eject_cassette()} on the
last line.

Run the example manually once to record the vignettte, then it will be
replayed during \verb{R CMD check}, ensuring that your example no longer uses
the internet.
}
\examples{
\dontshow{if (requireNamespace("httr2")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
# In this example I'm showing the insert and eject commands, but you'd
# usually wrap these in \dontshow{} so the user doesn't see them and
# think that they're something they need to copy.

insert_example_cassette("httpbin-get", package = "vcr")

req <- httr2::request("https://hb.cran.dev/get")
resp <- httr2::req_perform(req)

str(httr2::resp_body_json(resp))

eject_cassette()
\dontshow{\}) # examplesIf}
}
