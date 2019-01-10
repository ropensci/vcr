vcr
===



[![cran checks](https://cranchecks.info/badges/worst/vcr)](https://cranchecks.info/pkgs/vcr)
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![Build Status](https://travis-ci.org/ropensci/vcr.svg)](https://travis-ci.org/ropensci/vcr)
[![codecov](https://codecov.io/gh/ropensci/vcr/branch/master/graph/badge.svg)](https://codecov.io/gh/ropensci/vcr)
[![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/vcr)](https://github.com/metacran/cranlogs.app)
[![cran version](https://www.r-pkg.org/badges/version/vcr)](https://cran.r-project.org/package=vcr)

An R port of the Ruby gem [vcr](https://github.com/vcr/vcr)

## Docs

Check out the [HTTP testing book](https://ropensci.github.io/http-testing-book/) and the [vcr vignettes](vignettes).

## Supported HTTP libraries

* [crul](https://github.com/ropensci/crul)
* [httr](https://github.com/r-lib/httr)

## Usage




```r
library(vcr)
library(crul)

cli <- crul::HttpClient$new(url = "https://eu.httpbin.org")
system.time(
  use_cassette(name = "helloworld", {
    cli$get("get")
  })
)
#>    user  system elapsed 
#>   0.168   0.023   1.175
```

The request gets recorded, and all subsequent requests of the same form used the cached HTTP response, and so are much faster


```r
system.time(
  use_cassette(name = "helloworld", {
    cli$get("get")
  })
)
#>    user  system elapsed 
#>   0.080   0.004   0.085
```



Importantly, your unit test deals with the same inputs and the same outputs - but behind the scenes you use a cached HTTP response - thus, your tests run faster.

The cached response looks something like (condensed for brevity):

```yaml
http_interactions:
- request:
    method: get
    uri: https://eu.httpbin.org/get
    body:
      encoding: ''
      string: ''
    headers:
      User-Agent: libcurl/7.54.0 r-curl/3.2 crul/0.5.2
  response:
    status:
      status_code: '200'
      message: OK
      explanation: Request fulfilled, document follows
    headers:
      status: HTTP/1.1 200 OK
      connection: keep-alive
    body:
      encoding: UTF-8
      string: "{\n  \"args\": {}, \n  \"headers\": {\n    \"Accept\": \"application/json,
        text/xml, application/xml, */*\", \n    \"Accept-Encoding\": \"gzip, deflate\",
        \n    \"Connection\": \"close\", \n    \"Host\": \"httpbin.org\", \n    \"User-Agent\":
        \"libcurl/7.54.0 r-curl/3.2 crul/0.5.2\"\n  }, \n  \"origin\": \"111.222.333.444\",
        \n  \"url\": \"https://eu.httpbin.org/get\"\n}\n"
  recorded_at: 2018-04-03 22:55:02 GMT
  recorded_with: vcr/0.1.0, webmockr/0.2.4, crul/0.5.2
```

All components of both the request and response are preserved, so that the HTTP client (in this case `crul`) can reconstruct its own response just as it would if it wasn't using `vcr`.

## Terminology

* _vcr_: the name comes from the idea that we want to record something and play it back later, like a vcr
* _cassette_: A _thing_ to record HTTP interactions to. Right now the only option is file system, but in the future could be other things, e.g. a key-value store like Redis
* Persisters: defines how to save requests - currently only option is the file system
* Serializers: defines how to serialize the HTTP response - currently only option is YAML; other options in the future could include e.g. JSON
* _insert cassette_: create a cassette (all HTTP interactions will be recorded to this cassette)
* _eject cassette_: eject the cassette (no longer recording to that cassette)
* _replay_: refers to using a cached result of an http request that was recorded earlier

## What does vcr do?

The short version is: `vcr` helps you stub HTTP requests so you don't have to repeat HTTP requests.

The main use case is for unit tests for R packages.

`vcr` currently works with the `crul` and `httr` packages; support for `curl` is in the works.

### How it works in lots of detail

**The Steps**

1. Use either `vcr::use_cassette` or `vcr::insert_cassette`
  a. If you use `vcr::insert_cassette`, make sure to run `vcr::eject_cassette` when you're done to stop recording
2. When you first run a request with `vcr` there's no cached data to use, so we allow HTTP requests until you're request is done.
3. Before we run the real HTTP request, we "stub" the request with `webmockr` so that future requests will match the stub.
This stub is an R6 class with details of the interaction (request + response), but is not on disk.
4. After the stub is made, we run the real HTTP request.
5. We then disallow HTTP requests so that if the request is done again we use the cached response
6. The last thing we do is write the HTTP interaction to disk in a mostly human readable form.

When you run that request again using `vcr::use_cassette` or `vcr::insert_cassette`:

* We use `webmockr` to match the request to cached requests, and since we stubbed the request the first time we used the cached response.

Of course if you do a different request, even slightly (but depending on which matching format you decided to use), then
the request will have no matching stub and no cached response, and then a real HTTP request is done - we then cache it, then subsequent requests will pull from that cached response.

`webmockr` has adapters for each R client (again, right now only [crul][]) - so that we actually intercept HTTP requests when `webmockr` is loaded and the user turns it on. So, `webmockr` doesn't actually require an internet or localhost connection at all, but can do its thing just fine by matching on whatever the user requests to match on. In fact, `webmockr` doesn't allow real HTTP requests by default, but can be toggled off of course.

The main use case we are going for in `vcr` is to deal with real HTTP requests and responses, so we allow real HTTP requests when we need to, and turn it off when we don't.

This gives us a very flexible and powerful framework where we can support `webmockr` and `vcr` integration for any number of R clients for HTTP requests and support many different formats serialized to disk.



### Just want to mock and not store on disk?

You're looking for [webmockr][]. `webmockr` only matches requests based on criteria you choose, but does not cache HTTP interactions to disk as `vcr` does.

<br>

## Best practices

### vcr for tests

* Add `vcr` to `Suggests` in your DESCRIPTION file (optionally add `webmockr`, but it's not explicitly needed as `vcr` will pull it in) 
* Make a file in your `tests/testthat/` directory called `helper-yourpackage.R` (or skip if as similar file already exists). In that file use the following lines to setup your path for storing cassettes (change path to whatever you want):

```r
library("vcr")
invisible(vcr::vcr_configure())
```

* In your tests, for whichever tests you want to use `vcr`, wrap them in a `vcr::use_cassette()` call like:

```r
library(testthat)
vcr::use_cassette("rl_citation", {
  test_that("my test", {
    aa <- rl_citation()

    expect_is(aa, "character")
    expect_match(aa, "IUCN")
    expect_match(aa, "www.iucnredlist.org")
  })
})
```

**Important**: If you wrap your `test_that()` block inside your `use_cassette()` block you'll get the correct
line numbers from `testthat` when there are errors/warnings/etc. However, if you wrap the `use_cassette()` block inside your  `test_that()` block, on errors/etc. you'll only get the line number that that `use_cassette()` block starts on, 
which is only identifies the code block but not the offending line itself.

* When running tests or checks of your whole package, note that some users have found different results with 
`devtools::check()` vs. `devtools::test()`. It's not clear why this would make a difference. Do let us know 
if you run into this problem.

### vcr in your R project

You can use `vcr` in an R project as well.

* Load `vcr` in your project
* Similar to the above example, use `use_cassette` to run code that does HTTP requests.
* The first time a real request is done, and after that the cached response will be used.


## Installation

CRAN version:


```r
install.packages("vcr")
```

Development version:


```r
devtools::install_github("ropensci/vcr")
```


```r
library("vcr")
library("crul")
```

## Configuration

We set the following defaults:

* `dir` = "."
* `record` = "once"
* `match_requests_on` = `c("method", "uri")`
* `allow_unused_http_interactions` = `TRUE`
* `serialize_with` = `"yaml"`
* `persist_with` = `"FileSystem"`
* `ignore_hosts` = `NULL`
* `ignore_localhost` = `FALSE`
* `ignore_request` = `NULL`
* `uri_parser` = `crul::url_parse`
* `preserve_exact_body_bytes` = `FALSE`
* `turned_off` = `FALSE`
* `ignore_cassettes` = `FALSE`
* `re_record_interval` = `NULL`
* `clean_outdated_http_interactions` = `NULL`
* `cassettes` = `list()`
* `linked_context` = `NULL`
* `vcr_logging` = `"vcr.log"`
* `vcr_logging_opts` = `list()`


You can get the defaults programmatically with

```r
vcr_config_defaults()
```

You can change all the above defaults with `vcr_configure()`:

```r
vcr_configure()
```

Calling `vcr_configuration()` gives you some of the more important defaults in a nice tidy print out

```r
vcr_configuration()
```


<!-- `use_cassette()` is an easier approach. An alternative is to use
`insert_cassett()` + `eject_cassette()`.

`use_cassette()` does both insert and eject operations for you, but
you can instead do them manually by using the above functions. You do have
to eject the cassette after using insert. -->

## Matching/Matchers

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

By default, we use `method` (HTTP method, e.g., `GET`) and `uri` (test for exact match against URI, e.g., `http://foo.com`).

You can set your own options by tweaking the `match_requests_on` parameter:




```r
use_cassette(name = "one", {
    cli$post("post", body = list(a = 5))
  },
  match_requests_on = c('method', 'headers', 'body')
)
```

## vcr in other languages

The canonical `vcr` (in Ruby) lists ports in other languages at <https://github.com/vcr/vcr>

## NOTE

There's a number of features in this package that are not yet supported, but for which their parameters are found in the package. For example, `decode_compressed_response` is a parameter in `use_cassette()` but it is ignored right now.

We've tried to make sure the parameters that are ignored are marked as such. Keep an eye out for package updates for changes in these parameters, and/or let us know you want it and we can move it up in the priority list.

## Example packages using vcr

* [rgbif][]
* [rredlist][]
* [bold][]
* [wikitaxa][]
* [worrms][]
* [microdemic][]
* [zbank][]
* [rplos][]
* [ritis][]
* [nasapower][]

## TODO

* Logging
* Provide toggling a re-record interval so you can say e.g., after 6 hrs, re-record a real response, updating the cached response
* ...

## Meta

* Please [report any issues or bugs](https://github.com/ropensci/vcr/issues)
* License: MIT
* Get citation information for `vcr` in R doing `citation(package = 'vcr')`
* Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

[![ropensci_footer](https://ropensci.org/public_images/github_footer.png)](https://ropensci.org)

[webmockr]: https://github.com/ropensci/webmockr
[crul]: https://github.com/ropensci/crul
[rgbif]: https://github.com/ropensci/rgbif
[rdatacite]: https://github.com/ropensci/rdatacite  
[rredlist]: https://github.com/ropensci/rredlist
[bold]: https://github.com/ropensci/bold
[wikitaxa]: https://github.com/ropensci/wikitaxa
[worrms]: https://github.com/ropensci/worrms
[microdemic]: https://github.com/ropensci/microdemic
[zbank]: https://github.com/ropenscilabs/zbank
[rplos]: https://github.com/ropensci/rplos
[ritis]: https://github.com/ropensci/ritis
[nasapower]: https://github.com/ropensci/nasapower
