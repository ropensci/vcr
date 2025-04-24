# request_summary works

    Code
      cat(request_summary(request, c("method", "uri")))
    Output
      post http://example.com
    Code
      cat(request_summary(request, c("method", "uri", "body")))
    Output
      post http://example.com body
    Code
      cat(request_summary(request, c("method", "uri", "headers")))
    Output
      post http://example.com 1 2
    Code
      cat(request_summary(request, c("method", "uri", "body", "headers")))
    Output
      post http://example.com body 1 2

# response_summary works

    Code
      response_summary(response)
    Output
      [1] "200 bodybodybodybodybodybodybodybodybodybodybodybodybodybodybodybodybodybodybodybody"

# response_summary works with raw body

    Code
      response_summary(response)
    Output
      [1] "200 <raw>"

# response_summary - handles bad multibyte characters by changing encoding

    Code
      response_summary(response)
    Output
      [1] "200 "

