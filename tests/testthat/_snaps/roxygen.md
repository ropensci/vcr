# @examplesVCR

    Code
      out$get_section("examples")
    Output
      \examples{
      \dontshow{vcr::insert_example_cassette('bla', package = 'my.package')}
      httr2::request('http://r-project.org')
      \dontshow{vcr::eject_cassette()}
      } 

