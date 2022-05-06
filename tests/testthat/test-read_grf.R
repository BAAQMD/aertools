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

  result <- read_grf(
    path,
    cols = cols,
    crs = "epsg:26910",
    as = "sf")

  expect_setequal(
    names(result),
    c("PERIOD_MAXPAK1", "PERIOD_MAXPAK2", "PERIOD_NEOTEC1", "PERIOD_NEOTEC2",
      "PERIOD_NEOTEC3", "PERIOD_NEOTEC4", "PERIOD_OXIDIZER", "PERIOD_FIREPUMP", "PERIOD_ALL",
      "1HR_MAXPAK1", "1HR_MAXPAK2", "1HR_NEOTEC1", "1HR_NEOTEC2",
      "1HR_NEOTEC3", "1HR_NEOTEC4", "1HR_OXIDIZER", "1HR_FIREPUMP", "1HR_ALL"))

  expect_snapshot(digest::digest(result[[1]], algo = "md5"))
  expect_s3_class(result[[1]], "sf")

})
