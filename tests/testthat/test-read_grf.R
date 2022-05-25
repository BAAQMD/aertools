options(digits = 9, width = 80)

path <-
  system.file(
    "extdata",
    "test-HRSA.GRF",
    package = "aertools",
    mustWork = TRUE)

cols <- c(
  X = "X",
  Y = "Y",
  CONC = "AVERAGE CONC")

test_that("read_grf(path, ...) yields expected snapshot", {

  aer_ids <- str_c(
    "PERIOD_",
    c("MAXPAK1", "MAXPAK2", "FIREPUMP"))

  result <- read_grf(
    path,
    ids = aer_ids,
    cols = cols)

  expect_setequal(
    names(result),
    aer_ids)

  expect_snapshot(digest::digest(result[[1]], algo = "md5"))
  expect_s3_class(result[[1]], "data.frame")

  expect_equal(
    nrow(result[[1]]),
    4378)

  expect_setequal(
    names(result[[1]]),
    c("path", "X", "Y", "CONC"))

})

test_that("read_grf(path, ...) meets expectations for specific values", {

  result <- read_grf(
    path,
    ids = c("PERIOD_MAXPAK1", "PERIOD_MAXPAK2"),
    cols = cols)

  expect_equal(
    head(result[["PERIOD_MAXPAK1"]][["CONC"]], 1),
    0.44831)

  expect_equal(
    tail(result[["PERIOD_MAXPAK1"]][["CONC"]], 1),
    0.02717)

  expect_equal(
    head(result[["PERIOD_MAXPAK2"]][["CONC"]], 1),
    0.57690)

})

