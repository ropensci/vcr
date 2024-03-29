% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/serializers.R
\name{Serializers}
\alias{Serializers}
\alias{serializer_fetch}
\title{Cassette serializers}
\usage{
serializer_fetch(x = "yaml", name)
}
\description{
Keeps track of the cassette serializers in a hash-like object
}
\details{
\strong{Private Methods}
\describe{
\item{\code{serialize_get()}}{
Gets a named serializer. This is also run on \code{Serializers$new()}
}
}
}
\examples{
\dontrun{
(aa <- Serializers$new())
aa$name
aa$serializers
yaml_serializer <- aa$serializers$new()
yaml_serializer

x <- Serializers$new(name = "json")
x$serializers$new()
json_serializer <- x$serializers$new()
json_serializer
}
}
\keyword{internal}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{serializers}}{(list) list of serializer names}

\item{\code{name}}{(character) Name of a serializer. "yaml" (default) or "json"}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-Serializers-new}{\code{Serializers$new()}}
\item \href{#method-Serializers-clone}{\code{Serializers$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Serializers-new"></a>}}
\if{latex}{\out{\hypertarget{method-Serializers-new}{}}}
\subsection{Method \code{new()}}{
Create a new Serializers object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Serializers$new(serializers = list(), name = "yaml")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{serializers}}{(list) list of serializer names}

\item{\code{name}}{(character) Name of a serializer. "yaml" (default) or "json"}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new \code{Serializers} object
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Serializers-clone"></a>}}
\if{latex}{\out{\hypertarget{method-Serializers-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Serializers$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
