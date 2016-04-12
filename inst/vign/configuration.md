<!--
%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{vcr configuration}
%\VignetteEncoding{UTF-8}
-->



vr configuration
================

`vcr` configuration


```r
library("vcr")
```

## Get configuration

Use `vcr_configuration()` to get the current configuration


```r
vcr_configuration()
#> <vcr configuration>
#>   Cassette Dir: ~/vcr/vcr_cassettes
#>   Record: once
#>   URI Parser: httr::parse_url
#>   Match Requests on: method, uri
```

You can get the default configuration variables via `vcr_config_defaults()`


```r
vcr_config_defaults()
#> $dir
#> [1] "~/vcr/vcr_cassettes"
#> 
#> $record
#> [1] "once"
#> 
#> $match_requests_on
#> [1] "method" "uri"   
#> 
#> $allow_unused_http_interactions
#> [1] TRUE
#> 
#> $serialize_with
#> [1] "yaml"
#> 
#> $persist_with
#> [1] "FileSystem"
#> 
#> $ignore_hosts
#> NULL
#> 
#> $ignore_localhost
#> [1] FALSE
#> 
#> $ignore_request
#> NULL
#> 
#> $uri_parser
#> [1] "httr::parse_url"
#> 
#> $preserve_exact_body_bytes
#> [1] FALSE
#> 
#> $preserve_exact_body_bytes_for
#> [1] FALSE
#> 
#> $turned_off
#> [1] FALSE
#> 
#> $ignore_cassettes
#> [1] FALSE
#> 
#> $cassettes
#> list()
#> 
#> $linked_context
#> NULL
```

These defaults are set when you load `vcr` - you can override any of them as described below.

## Set configuration variables

Use `vcr_configure()` to set configuration variables.

For example, set a single variable:


```r
vcr_configure(
  dir = "foobar/vcr_cassettes"
)
#> <vcr configuration>
#>   Cassette Dir: foobar/vcr_cassettes
#>   Record: once
#>   URI Parser: httr::parse_url
#>   Match Requests on: method, uri
```

Or many at once:


```r
vcr_configure(
  dir = "foobar/vcr_cassettes",
  record = "all"
)
#> <vcr configuration>
#>   Cassette Dir: foobar/vcr_cassettes
#>   Record: all
#>   URI Parser: httr::parse_url
#>   Match Requests on: method, uri
```

## Re-set to defaults


```r
vcr_configure_reset()
#> <vcr configuration>
#>   Cassette Dir: ~/vcr/vcr_cassettes
#>   Record: once
#>   URI Parser: httr::parse_url
#>   Match Requests on: method, uri
```

## Individual config options

### dir - directory of where cassettes are stored


```r
vcr_configure(dir = "new/path")
#> <vcr configuration>
#>   Cassette Dir: new/path
#>   Record: once
#>   URI Parser: httr::parse_url
#>   Match Requests on: method, uri
```

### record - record mode

One of: 'all', 'none', 'new_episodes', 'once'. See `?recording` for info on the options


```r
vcr_configure(record = "new_episodes")
#> <vcr configuration>
#>   Cassette Dir: ~/vcr/vcr_cassettes
#>   Record: new_episodes
#>   URI Parser: httr::parse_url
#>   Match Requests on: method, uri
```

### match_requests_on - customize how vcr matches requests


```r
vcr_configure(match_requests_on = c('query', 'headers'))
#> <vcr configuration>
#>   Cassette Dir: ~/vcr/vcr_cassettes
#>   Record: once
#>   URI Parser: httr::parse_url
#>   Match Requests on: query, headers
```

### allow_unused_http_interactions - Allow HTTP connections when no cassette

Default is `TRUE`, and thus does not error when http interactions are unused. You 
can set to `FALSE` in which case vcr errors when a cassette is ejected and 
not all http interactions have been used.


```r
vcr_configure(allow_unused_http_interactions = FALSE)
#> <vcr configuration>
#>   Cassette Dir: ~/vcr/vcr_cassettes
#>   Record: once
#>   URI Parser: httr::parse_url
#>   Match Requests on: method, uri
```

### serialize_with - which serializer to use

Right now only option is "yaml"


```r
vcr_configure(serialize_with = "yaml")
#> <vcr configuration>
#>   Cassette Dir: ~/vcr/vcr_cassettes
#>   Record: once
#>   URI Parser: httr::parse_url
#>   Match Requests on: method, uri
```

### persist_with - which persister to use

Right now only option is "FileSystem"


```r
vcr_configure(persist_with = "FileSystem")
#> <vcr configuration>
#>   Cassette Dir: ~/vcr/vcr_cassettes
#>   Record: once
#>   URI Parser: httr::parse_url
#>   Match Requests on: method, uri
```

### ignore_hosts - specify particular hosts to ignore


```r
vcr_configure(ignore_hosts = "google.com")
#> <vcr configuration>
#>   Cassette Dir: ~/vcr/vcr_cassettes
#>   Record: once
#>   URI Parser: httr::parse_url
#>   Match Requests on: method, uri
```

### ignore_localhost - ignore all localhost flavors


```r
vcr_configure(ignore_localhost = TRUE)
#> <vcr configuration>
#>   Cassette Dir: ~/vcr/vcr_cassettes
#>   Record: once
#>   URI Parser: httr::parse_url
#>   Match Requests on: method, uri
```

### ignore_request - ignore any request for which function is true


```r
vcr_configure(ignore_request = function(x) x == 5)
#> <vcr configuration>
#>   Cassette Dir: ~/vcr/vcr_cassettes
#>   Record: once
#>   URI Parser: httr::parse_url
#>   Match Requests on: method, uri
```

### uri_parser - which uri parser to use

By default we use `httr::parse_url`, but you can use a different one. Remember 
to pass in the function quoted, and namespaced.


```r
vcr_configure(uri_parser = "urltools::url_parse")
#> <vcr configuration>
#>   Cassette Dir: ~/vcr/vcr_cassettes
#>   Record: once
#>   URI Parser: urltools::url_parse
#>   Match Requests on: method, uri
```

### preserve_exact_body_bytes

Some HTTP servers are not well-behaved and respond with invalid data. Set 
`preserve_exact_body_bytes` to `TRUE` to base64 encode the result body in 
order to preserve the bytes exactly as-is. `vcr` does not do this by
default, since base64-encoding the string removes the human readibility 
of the cassette.


```r
vcr_configure(preserve_exact_body_bytes = TRUE)
#> <vcr configuration>
#>   Cassette Dir: ~/vcr/vcr_cassettes
#>   Record: once
#>   URI Parser: httr::parse_url
#>   Match Requests on: method, uri
```
