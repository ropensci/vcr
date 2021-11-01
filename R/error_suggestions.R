error_suggestions <- list(
  use_new_episodes = list(
    text = c("You can use the :new_episodes record mode to allow vcr to",
     "record this new request to the existing cassette"),
    url = "https://books.ropensci.org/http-testing/record-modes.html#new_episodes"
  ),

  delete_cassette_for_once = list(
    text = c("The current record mode ('once') does not allow new requests to be recorded",
     "to a previously recorded cassette. You can delete the cassette file and re-run",
     "your tests to allow the cassette to be recorded with this request"),
     url = "https://books.ropensci.org/http-testing/record-modes.html#once"
  ),

  deal_with_none = list(
    text = c("The current record mode ('none') does not allow requests to be recorded. You",
     "can temporarily change the record mode to :once, delete the cassette file ",
     "and re-run your tests to allow the cassette to be recorded with this request"),
     url = "https://books.ropensci.org/http-testing/record-modes.html#none"
  ),

  use_a_cassette = list(
    text = c("If you want vcr to record this request and play it back during future test",
     "runs, you should wrap your test (or this portion of your test) in a",
     "`vcr::use_cassette` block"),
    url = "https://books.ropensci.org/http-testing/intro"
  ),

  allow_http_connections_when_no_cassette = list(
    text = c("If you only want vcr to handle requests made while a cassette is in use,",
     "configure `allow_http_connections_when_no_cassette = TRUE`. vcr will",
     "ignore this request since it is made when there is no cassette"),
    url = "https://books.ropensci.org/http-testing/vcr-configuration#allow-http-connections-when-no-cassette"
  ),

  ignore_request = list(
    text = c("If you want vcr to ignore this request (and others like it), you can",
     "set an `ignore_request` function"),
    url = "https://books.ropensci.org/http-testing/vcr-configuration#config-ignore-requests"
  ),

  allow_playback_repeats = list(
    text = c("The cassette contains an HTTP interaction that matches this request,",
     "but it has already been played back. If you wish to allow a single HTTP",
     "interaction to be played back multiple times, set the `allow_playback_repeats`",
     "cassette option"),
    url = "https://books.ropensci.org/http-testing/request-matching#playback-repeats"
  ),

  match_requests_on = list(
    text = c("The cassette contains %s not been",
     "played back. If your request is non-deterministic, you may need to",
     "change your 'match_requests_on' cassette option to be more lenient",
     "or use a custom request matcher to allow it to match"),
     url = "https://books.ropensci.org/http-testing/request-matching"
  ),

  try_debug_logger = list(
    text = c("If you're surprised vcr is raising this error",
     "and want insight about how vcr attempted to handle the request,",
     "you can use 'logging' to see more details"),
    url = "https://books.ropensci.org/http-testing/debugging-your-tests-that-use-vcr.html#logging-1"
  )
)
