# can turn off and turn on

    Code
      turn_off()
    Message
      vcr turned off; see ?turn_on to turn vcr back on

# inserting a cassette errors when vcr turned off and ignore_cassettes=FALSE

    Code
      insert_cassette("test")
    Condition
      Error in `insert_cassette()`:
      ! vcr is turned off.  You must turn it on before you can insert a cassette.
              Or you can set ignore_cassettes=TRUE option to completely ignore cassette insertions.

# turned_off works iif no cassettes active

    Code
      turned_off(5 + 5)
    Condition
      Error in `turn_off()`:
      ! You must eject all cassettes before you can turn vcr off.

# lightswitch env var handles varying inputs

    Code
      lightswitch_init()
    Condition
      Error in `lightswitch_init()`:
      ! env var `VCR_TURN_OFF` must be TRUE or FALSE, not "yes".

