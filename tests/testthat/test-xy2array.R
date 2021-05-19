test_that("xy2array works on single PLT file", {

  path <- system.file("extdata", "PE_RAI1044_0-0.PLT", package = "aertools", mustWork = TRUE)
  dt <- read_plt(path, as = "data.table", .id = NULL)

  expect_setequal(names(dt), c("X", "Y", "CONC"))
  arr <- xy2array.data.table(dt)

  expect_equal(dim(arr), c(X = 129, Y = 149))
  expect_snapshot_value(arr, style = "deparse")

})
