# request_summary works

    Code
      cat(request_summary(request, c("method", "uri")))
    Output
      POST http://example.com
    Code
      cat(request_summary(request, c("method", "uri", "body")))
    Output
      POST http://example.com body
    Code
      cat(request_summary(request, c("method", "uri", "headers")))
    Output
      POST http://example.com 1 2
    Code
      cat(request_summary(request, c("method", "uri", "body", "headers")))
    Output
      POST http://example.com body 1 2

# response_summary works

    Code
      response_summary(response_raw)
    Output
      [1] "200 with 4 bytes of binary data"
    Code
      response_summary(response_unknown)
    Output
      [1] "200 with 400 bytes of text data"
    Code
      response_summary(response_json)
    Output
      [1] "200 with 400 bytes of application/json data"

