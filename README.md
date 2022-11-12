vcr
===



<!-- README.md is generated from README.Rmd. Please edit that file -->

[![cran checks](https://badges.cranchecks.info/worst/vcr.svg)](https://cloud.r-project.org/web/checks/check_results_vcr.html)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-check](https://github.com/ropensci/vcr/workflows/R-check/badge.svg)](https://github.com/ropensci/vcr/actions/)
[![codecov](https://codecov.io/gh/ropensci/vcr/branch/main/graph/badge.svg)](https://codecov.io/gh/ropensci/vcr)
[![rstudio mirror downloads](https://cranlogs.r-pkg.org/badges/vcr)](https://github.com/r-hub/cranlogs.app)
[![cran version](https://www.r-pkg.org/badges/version/vcr)](https://cran.r-project.org/package=vcr)


Easier HTTP testing! Record HTTP requests and responses on disk and replay them for the unit tests of your R package, to make them independent from any connection, faster, and more complete. An R port of the Ruby gem [vcr](https://github.com/vcr/vcr)

## Elevator pitch


* **Setup vcr for your package with `vcr::use_vcr()`**
* Tweak the configuration to protect your secrets
* **Sprinkle your tests with `vcr::use_cassette()` to save HTTP interactions to disk in "cassettes" files**
* If you want to test for package behavior when the API returns e.g. a 404 or 503 code, edit the cassettes, or use [webmockr](https://docs.ropensci.org/webmockr/)

Now your tests can work without any internet connection!

[Demo of adding vcr testing to an R package](https://github.com/maelle/exemplighratia/pull/2/files), [corresponding narrative](https://books.ropensci.org/http-testing/vcr.html).

## Installation

CRAN version:


```r
install.packages("vcr")
```

Development version:


```r
remotes::install_github("ropensci/vcr")
```


```r
library("vcr")
library("crul")
```


## Docs

Check out the [HTTP testing book](https://books.ropensci.org/http-testing) and the [vcr vignettes](https://docs.ropensci.org/vcr/articles/).

## Supported HTTP libraries

* [crul](https://docs.ropensci.org/crul/)
* [httr](https://httr.r-lib.org/)

## Getting Started


The docs assume you are using testthat for your unit tests.

### `use_vcr`

You can then set up your package to use `vcr` with:

```r
vcr::use_vcr()
```

This will:

* put `vcr` into the `DESCRIPTION`
* check that `testthat` is setup
* setup `testthat` if not
* set the recorded cassettes to be saved in and sourced from `tests/fixtures`
* setup a config file for `vcr`
* add an example test file for `vcr`
* make a `.gitattributes` file with settings for `vcr` 
* make a `./tests/testthat/setup-vcr.R` file

What you will see in the R console:

```
◉ Using package: vcr.example  
◉ assuming fixtures at: tests/fixtures  
✓ Adding vcr to Suggests field in DESCRIPTION  
✓ Creating directory: ./tests/testthat  
◉ Looking for testthat.R file or similar  
✓ tests/testthat.R: added  
✓ Adding vcr config to tests/testthat/setup-vcr.example.R  
✓ Adding example test file tests/testthat/test-vcr_example.R  
✓ .gitattributes: added  
◉ Learn more about `vcr`: https://books.ropensci.org/http-testing
```

### Protecting secrets

Secrets often turn up in API work. A common example is an API key. 
`vcr` saves responses from APIs as YAML files, and this will include your secrets unless you indicate to `vcr` what they are and how to protect them.
The `vcr_configure` function has the `filter_sensitive_data` argument function for just this situation. 
The `filter_sensitive_data` argument takes a named list where the _name_ of the list is the string that will be used in the recorded cassettes _instead of_ the secret, which is the list _item_. 
`vcr` will manage the replacement of that for you, so all you need to do is to edit your `setup-vcr.R` file like this:

```r
library("vcr") # *Required* as vcr is set up on loading
invisible(vcr::vcr_configure(
  dir = "../fixtures"
))
vcr::check_cassette_names()
```

Use the `filter_sensitive_data` argument in the `vcr_configure` function to show `vcr` how to keep your secret. The best way to store secret information is to have it in a `.Renviron` file. Assuming that that is already in place, supply a named list to the `filter_sensitive_data` argument.

```r
library("vcr")
invisible(vcr::vcr_configure(
  filter_sensitive_data = list("<<<my_api_key>>>" = Sys.getenv('APIKEY')),  # add this
  dir = "../fixtures"
))
vcr::check_cassette_names()
```

Notice we wrote `Sys.getenv('APIKEY')` and not the API key directly, otherwise you'd have written your API key to a file that might end up in a public repo.

The will get your secret information from the environment, and make sure that whenever `vcr` records a new cassette, it will replace the secret information with `<<<my_api_key>>>`. You can find out more about this in the [HTTP testing book](https://books.ropensci.org/http-testing/) chapter on security.

The addition of the line above will instruct `vcr` to replace any string in cassettes it records that are equivalent to your string which is stored as the `APIKEY` environmental variable with the masking string `<<<my_api_key>>>`. In practice, you might get a `YAML` that looks a little like this:

```yaml
http_interactions:
- request:
    method: post
    ...
    headers:
      Accept: application/json, text/xml, application/xml, */*
      Content-Type: application/json
      api-key: <<<my_api_key>>>
    ...
```
Here, my `APIKEY` environmental variable would have been stored as the `api-key` value, but `vcr` has realised this and recorded the string `<<<my_api_key>>>` instead.

Once the cassette is recorded, `vcr` no longer needs the API key as no real requests will be made.
Furthermore, as by default requests matching does not include the API key, things will work.

**Now, how to ensure tests work in the absence of a real API key?**

E.g. to have tests pass on continuous integration for external pull requests to your code repository.

* vcr does not need an actual API key for requests once the cassettes are created, as no real requests will be made.
* you still need to fool your _package_ into believing there is an API key as it will construct requests with it. So add the following lines to a testthat setup file (e.g. `tests/testthat/setup-vcr.R`)

```r
if (!nzchar(Sys.getenv("APIKEY"))) {
  Sys.setenv("APIKEY" = "foobar")
}
```

#### Using an `.Renviron`

A simple way to manage local environmental variables is to use an [`.Renviron` file](https://rstats.wtf/r-startup.html#renviron). 
Your `.Renviron` file might look like this:

```sh
APIKEY="mytotallysecretkey"
```

You can have this set at a project or user level, and `usethis` has the [`usethis::edit_r_environ()`](https://usethis.r-lib.org/reference/edit.html) function to help edit the file.

## Usage





### In tests

In your tests, for whichever tests you want to use `vcr`, wrap them in a `vcr::use_cassette()` call like:

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

OR put the `vcr::use_cassette()` block on the inside, but put `testthat` expectations outside of
the `vcr::use_cassette()` block:

```r
library(testthat)
test_that("my test", {
  vcr::use_cassette("rl_citation", {
    aa <- rl_citation()
  })

  expect_is(aa, "character")
  expect_match(aa, "IUCN")
  expect_match(aa, "www.iucnredlist.org")
})
```

Don't wrap the `use_cassette()` block inside your  `test_that()` block with `testthat` expectations inside the `use_cassette()` block, as you'll only get the line number that the `use_cassette()` block starts on on failures.

The first time you run the tests, a "cassette" i.e. a file with recorded HTTP interactions, is created at `tests/fixtures/rl_citation.yml`.
The times after that, the cassette will be used.
If you change your code and more HTTP interactions are needed in the code wrapped by `vcr::use_cassette("rl_citation"`, delete `tests/fixtures/rl_citation.yml` and run the tests again for re-recording the cassette.

### Outside of tests

If you want to get a feel for how vcr works, although you don't need too.




```r
library(vcr)
library(crul)

cli <- crul::HttpClient$new(url = "https://eu.httpbin.org")
system.time(
  use_cassette(name = "helloworld", {
    cli$get("get")
  })
)
```

The request gets recorded, and all subsequent requests of the same form used the cached HTTP response, and so are much faster


```r
system.time(
  use_cassette(name = "helloworld", {
    cli$get("get")
  })
)
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


### Less basic usage

For tweaking things to your needs, make sure to read the docs about [configuration](https://docs.ropensci.org/vcr/articles/configuration.html) (e.g., where are the fixtures saved? can they be re-recorded automatically regulary?) and [request matching](https://docs.ropensci.org/vcr/articles/request_matching.html) (how does vcr match a request to a recorded interaction?)


## Terminology


* _vcr_: the name comes from the idea that we want to record something and play it back later, like a vcr
* _cassette_: A _thing_ to record HTTP interactions to. Right now the only option is the file system (writing to files), but in the future could be other things, e.g. a key-value store like Redis
* _fixture_: A fixture is something used to consistently test a piece of software. In this case, a cassette (just defined above) is a fixture - used in unit tests. If you use our setup function `vcr_setup()` the default directory created to hold cassettes is called `fixtures/` as a signal as to what the folder contains.
* Persisters: how to save requests - currently only option is the file system
* _serialize_: translating data into a format that can be stored; here, translate HTTP request and response data into a representation on disk to read back later
* Serializers: how to serialize the HTTP response - currently only option is YAML; other options in the future could include e.g. JSON
* _insert cassette_: create a cassette (all HTTP interactions will be recorded to this cassette)
* _eject cassette_: eject the cassette (no longer recording to that cassette)
* _replay_: refers to using a cached result of an http request that was recorded earlier

## Workflows



### vcr for tests

* See [usage section](#usage)

* When running tests or checks of your whole package, note that some users have found different results with
`devtools::check()` vs. `devtools::test()`. It's not clear why this would make a difference. Do let us know
if you run into this problem.

### vcr in your R project

You can use `vcr` in any R project as well.

* Load `vcr` in your project
* Similar to the above example, use `use_cassette` to run code that does HTTP requests.
* The first time a real request is done, and after that the cached response will be used.

### How it works in lots of detail

See the [vignette about internals](https://docs.ropensci.org/vcr/articles/internals.html)

### Just want to mock and not store on disk?

You're looking for [webmockr][], that vcr itself uses. 
`webmockr` only matches requests based on criteria you choose, but does not cache HTTP interactions to disk as `vcr` does.

## Configuration


See also the [configuration vignette](https://docs.ropensci.org/vcr/articles/configuration.html).

We set the following defaults:

* dir = `"."`
 * record = `"once"`
 * match_requests_on = `"c("method", "uri")"`
 * allow_unused_http_interactions = `TRUE`
 * serialize_with = `"yaml"`
 * json_pretty = `FALSE`
 * persist_with = `"FileSystem"`
 * ignore_hosts = `NULL`
 * ignore_localhost = `FALSE`
 * ignore_request = `NULL`
 * uri_parser = `"crul::url_parse"`
 * preserve_exact_body_bytes = `FALSE`
 * turned_off = `FALSE`
 * re_record_interval = `NULL`
 * clean_outdated_http_interactions = `FALSE`
 * allow_http_connections_when_no_cassette = `FALSE`
 * cassettes = `list()`
 * linked_context = `NULL`
 * log = `FALSE`
 * log_opts = `list(file = "vcr.log", log_prefix = "Cassette", date = TRUE)`
 * filter_sensitive_data = `NULL`
 * filter_sensitive_data_regex = `NULL`
 * filter_request_headers = `NULL`
 * filter_response_headers = `NULL`
 * filter_query_parameters = `NULL`
 * write_disk_path = `NULL`
 * verbose_errors = `FALSE`
 * quiet = `TRUE`
 * warn_on_empty_cassette = `TRUE`


You can get the defaults programmatically with

```r
vcr_config_defaults()
```

You can change all the above defaults with `vcr_configure()`:

```r
vcr_configure()
```

Calling `vcr_configuration()` gives you some of the more important configuration parameters in a nice tidy print out


```r
vcr_configuration()
#> <vcr configuration>
#>   Cassette Dir: .
#>   Record: once
#>   Serialize with: yaml
#>   URI Parser: crul::url_parse
#>   Match Requests on: method, uri
#>   Preserve Bytes?: FALSE
#>   Logging?: FALSE
#>   ignored hosts: 
#>   ignore localhost?: FALSE
#>   Write disk path:
```


<!-- `use_cassette()` is an easier approach. An alternative is to use
`insert_cassett()` + `eject_cassette()`.

`use_cassette()` does both insert and eject operations for you, but
you can instead do them manually by using the above functions. You do have
to eject the cassette after using insert. -->

For more details refer to the [configuration vignette](https://docs.ropensci.org/vcr/articles/configuration.html)

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

For more details refer to the [request matching vignette](https://docs.ropensci.org/vcr/articles/request_matching.html).

## vcr in other languages

The canonical `vcr` (in Ruby) lists ports in other languages at <https://github.com/vcr/vcr>

## Note about missing features


There's a number of features in this package that are not yet supported, but for which their parameters are found in the package. 

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

## Contributors

* [Scott Chamberlain](https://github.com/sckott)
* [Aaron Wolen](https://github.com/aaronwolen)
* [Maëlle Salmon](https://github.com/maelle)

## Meta

* Please [report any issues or bugs](https://github.com/ropensci/vcr/issues)
* License: MIT
* Get citation information for `vcr` in R doing `citation(package = 'vcr')`
* Please note that this package is released with a [Contributor Code of Conduct](https://ropensci.org/code-of-conduct/). By contributing to this project, you agree to abide by its terms.

[webmockr]: https://docs.ropensci.org/webmockr/
[crul]: https://docs.ropensci.org/crul/
[rgbif]: https://github.com/ropensci/rgbif
[rredlist]: https://github.com/ropensci/rredlist
[bold]: https://github.com/ropensci/bold
[wikitaxa]: https://github.com/ropensci/wikitaxa
[worrms]: https://github.com/ropensci/worrms
[microdemic]: https://github.com/ropensci/microdemic
[zbank]: https://github.com/ropenscilabs/zbank
[rplos]: https://github.com/ropensci/rplos
[ritis]: https://github.com/ropensci/ritis
