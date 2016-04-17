# From http://stackoverflow.com/questions/475074/regex-to-parse-or-validate-base64-data
## from http://stackoverflow.com/a/475217/1091766 -
## ^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=)?$
## from http://stackoverflow.com/a/5885097/1091766 -
## ^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=|[A-Za-z0-9+/]{4})$

b64_bob  <- "(?:[A-Za-z0-9+/]{4}\\n?)*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=)"
b64_so1  <- "^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=)?$"
b64_so2  <- "^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=|[A-Za-z0-9+/]{4})$"
str <- "fartasdfadfaf asdfdaf asd fa df af a fd afd as df asdf afd das fas df asf a f asd"
x <- base64enc::base64encode(charToRaw(str))
grepl(b64_bob, x)
grepl(b64_so1, x)
grepl(b64_so2, x)

grepl(b64_bob, str)
grepl(b64_so1, str)
grepl(b64_so2, str)
