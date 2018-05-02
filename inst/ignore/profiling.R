library(testthat); devtools::load_all()
library(crul)
library(dataonecrul)
vcr_configure(
  dir = ".",
  log = TRUE,
  log_opts = list(file = "vcr.log", log_prefix = "Cassette")
)
unlink("~/fixtures/vcr_cassettes/d1getobject.yml")

node <- node("https://cn.dataone.org/cn/v2")
#system.time(resp <- get_object(node, "8f096d0c7e2f5962ce4828cc6ea59572"))

library(profvis)
profvis::profvis({
  use_cassette("d1getobject", {
    resp2 <- get_object(node, "8f096d0c7e2f5962ce4828cc6ea59572")
  })
})

system.time(
  use_cassette("d1getobject", {
    resp2 <- get_object(node, "8f096d0c7e2f5962ce4828cc6ea59572")
  })
)

# cli <- crul::HttpClient$new("https://httpbin.org")
# x <- insert_cassette(name = "aaaa")
# webmockr::webmockr_allow_net_connect()
# one <- cli$get("get", query = list(a = 5))
# two <- cli$get("get", query = list(b = 6))
# eject_cassette()

#

