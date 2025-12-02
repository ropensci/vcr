test_that("insert_example_cassette works as expected", {
  withr::with_envvar(new = c("VCR_TURN_OFF" = "true"), {
    # Reinitialize the lightswitch to pick up the new env var
    the$light_switch <- lightswitch_init()

    # Verify VCR is actually turned off
    expect_true(vcr_turned_off())

    # insert_example_cassette shouldn't actually create a file
    # under this test scenario
    inserted_cas <- insert_example_cassette("fiz-baz", package = "vcr")
    req <- httr2::request("https://hb.cran.dev/get")
    resp <- httr2::req_perform(req)
    ejected_cas <- eject_cassette()

    expect_null(inserted_cas)
    expect_null(ejected_cas)
  })

  # reset the$light_switch
  the$light_switch <- lightswitch_init()
})
