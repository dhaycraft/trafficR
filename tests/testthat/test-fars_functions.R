
test_that("Error or warning  occurs when given incorrect year", {
  expect_error(fars_read(1990))
  expect_error(fars_summarize_years(1990))
  expect_warning(fars_read_years(1990:1991))
  expect_error(fars_map_state(state.num=6, year=1990))
})


test_that("Creates appropriate file name", {
  expect_match(make_filename(2013), "accident_2013.csv.bz2")
})


test_that("Check output class of output for each function", {
  expect_is(make_filename(2013), "character")
  expect_is(fars_read(make_filename(2013)), "tbl_df")
  expect_is(fars_read_years(2013:2014), "list")
  expect_is(fars_summarize_years(2013:2014), 'tbl_df')
})
