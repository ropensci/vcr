# has nice print method

    Code
      Cassette$new("test")
    Output
      <vcr - Cassette> test
        Record method: once
        Serialize with: yaml
        Re-record interval (s): 
        Clean outdated interactions?: FALSE
        allow_playback_repeats: FALSE
        preserve_exact_body_bytes: FALSE

# cassette warns if ejected with no interactions

    Code
      . <- cl$eject()
    Condition
      Warning:
      x "test" cassette ejected without recording any interactions.
      i Did you use {curl}, `download.file()`, or other unsupported tool?
      i If you are using crul/httr/httr2, are you sure you made an HTTP request?

