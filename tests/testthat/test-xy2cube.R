test_that("xy2cube works", {

  path <- system.file("extdata", "PE_RAI1044_0-0.PLT", package = "aertools", mustWork = TRUE)
  dt <- read_plt(path, as = "data.table", .id = NULL)

  expect_setequal(names(dt), c("X", "Y", "CONC"))
  cube <- xy2cube.data.table(dt)

  expect_snapshot(cube$dims)
  expect_snapshot(digest::digest(cube$mets, algo = "md5"))

  tbl <- as_tibble(cube)
  expect_equal(nrow(tbl), 129 * 149)
  expect_snapshot({set.seed(0); dplyr::sample_n(tbl, 10)})
  expect_snapshot(digest::digest(tbl, algo = "md5"))

})
