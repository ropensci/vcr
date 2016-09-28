vcr
===



[![Build Status](https://api.travis-ci.org/ropenscilabs/vcr.png?branch=master)](https://travis-ci.org/ropenscilabs/vcr)

An R port of the Ruby gem [vcr](https://github.com/vcr/vcr)

__>>>>>Not yet useable<<<<<__

## Overview/Terminology

* Cassette: A _thing_ to record HTTP interactions to. Right now the only option is file system, but in the future could be other things, e.g. a key-value store
* Persisters: defines how to save requests - currently only option is the file system
* Serializers: defines how to serialize the HTTP response - currently only option is YAML
* _insert cassette_: aka, create a cassette
* _eject cassette_: aka, check for saved interactions, and replay if found
* _replay_: refers to using a cached result of a http request that was recorded earlier
* How `vcr` matches: By default it matches on the HTTP method and the URI, but you can tweak this using the `match_requests_on` option.
* x

## Installation


```r
devtools::install_github("ropenscilabs/vcr")
```


```r
library("vcr")
library("httr")
```

## Best practices

* `vcr` for a package test suite 
  * put cassettes in a separate folder, and make sure to `.Rbuildignore` it - depending on its size you may want to add it to `.gitignore` as well. 
  * make sure to add `vcr` to `Suggests` in your package
  * If you want to clean up all cassettes at the end of the R session, run
  `xxx`
* `vcr` as a dependency in a package (i.e., Depends, Imports)
  * probably best to allow the user of your package to determine where cassettes are cached, so exposing `vcr` configuration tools to users
* `vcr` in your R analysis/project
  * xxx

## Configuration

Without the user doing anything, we set a number of defaults for easy usage:

* `dir` = "~/vcr/vcr_cassettes"
* `record` = "once"
* `match_requests_on` = `c("method", "uri")`
* `allow_unused_http_interactions` = `TRUE`
* `serialize_with` = "yaml"
* `persist_with` = "FileSystem"
* `ignore_hosts` = `NULL`
* `ignore_localhost` = `FALSE`
* `ignore_request` = `NULL`
* `uri_parser` = `httr::parse_url`
* `preserve_exact_body_bytes` = `FALSE`
* `preserve_exact_body_bytes_for` = `FALSE`
* `turned_off` = `FALSE`
* `ignore_cassettes` = `FALSE`
* `cassettes` = `list()` # empty set
* `linked_context` = `NULL`
* `vcr_logging` = "vcr.log"

You can get the defaults programatically with 


```r
vcr_config_defaults()
```

However, you can change all the above defaults via calling 
`vcr_configure()`


```r
vcr_configure(
  dir = "fixtures/vcr_cassettes",
  record = "all"
)
#> <vcr configuration>
#>   Cassette Dir: fixtures/vcr_cassettes
#>   Record: all
#>   URI Parser: httr::parse_url
#>   Match Requests on: method, uri
```

Calling `vcr_configuration()` gives you some of the more important defaults in a nice tidy print out


```r
vcr_configuration()
#> <vcr configuration>
#>   Cassette Dir: fixtures/vcr_cassettes
#>   Record: all
#>   URI Parser: httr::parse_url
#>   Match Requests on: method, uri
```


```
#> <vcr configuration>
#>   Cassette Dir: ~/vcr/vcr_cassettes
#>   Record: once
#>   URI Parser: httr::parse_url
#>   Match Requests on: method, uri
```

## Basic usage




```r
system.time(
  use_cassette("helloworld", {
    GET("http://localhost:9200/_search?size=900")
  })
)
#>    user  system elapsed 
#>   0.056   0.008   0.457
```

The request gets recorded, and all subsequent requests of the same form used the cached HTTP response, and so are much faster


```r
system.time(
  use_cassette("helloworld", {
    GET("http://localhost:9200/_search?size=900")
  })
)
#>    user  system elapsed 
#>   0.041   0.002   0.063
```

`use_cassette()` is an easier approach. An alternative is to use 
`insert_cassett()` + `eject_cassette()`. 

`use_cassette()` does both inject and eject operations for you, but 
you can instead do them manually by using the above functions. You do have
to eject the cassette after using inject.

## Matchers

`vcr` looks for similarity in your HTTP requests to cached requests. You 
can set what is examined about the request with one or more of the 
following options:

* `body`
* `headers`
* `host`
* `method`
* `path`
* `query`
* `uri`

By default, we use `method` (HTTP method, e.g., `GET`) and `uri` (test for exact match against URI). 

You can set your own options like:




```r
use_cassette(name = "one", {
    httr::GET("http://localhost:9200/_search?size=3", add_headers(a = 5))
  }, 
  match_requests_on = c('method', 'headers', 'query')
)
#> Response [http://localhost:9200/_search?size=3]
#>   Date: 2016-09-27 20:28
#>   Status: 200
#>   Content-Type: application/json; charset=UTF-8
#>   Size: 594 B
```

## Meta

* Please [report any issues or bugs](https://github.com/ropenscilabs/vcr/issues)
* License: MIT
* Get citation information for `vcr` in R doing `citation(package = 'vcr')`
* Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

[![ropensci_footer](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org)
