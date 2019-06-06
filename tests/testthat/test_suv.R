# test_suv.R

context("SUV calculation")

test_that("tac_suv() calculates SUV as expected", {

  f <- system.file("extdata", "AD06.tac", package="tacmagic")
  fv <- system.file("extdata", "AD06_TAC.voistat", package="tacmagic")
  AD06_tac <- load_tac(f, format="PMOD")
  AD06_volume <- load_vol(fv, format="voistat")
  AD06 <- tac_roi(tac=AD06_tac, volumes=AD06_volume, ROI_def=roi_ham_pib(),
                  merge=FALSE, PVC=FALSE)
  # dose and weight are fabricated for the example
  AD06_suv <- tac_suv(AD06, dose = 9.0, dose_unit = "mCi", weight_kg = 70)

  denominator <- change_units(9, from_unit = "mCi", to_unit = "MBq") / 70

  expect_equal(AD06_suv$totalcortical[2], AD06$totalcortical[2] / denominator)

  expect_equal(attributes(AD06_suv)$activity_unit, "g/mL")

  AD06b <- change_units(AD06, to_unit = "nCi/cc")
  AD06b_suv <- tac_suv(AD06b, dose = 333, dose_unit = "MBq", weight_kg = 70)

  denominator <- 333 / 70
  expect_equal(AD06b_suv$totalcortical[2], AD06$totalcortical[2] / denominator)

  not_tac <- data.frame(hi=NA)
  expect_error(tac_suv(not_tac, dose = 333, dose_unit = "MBq", weight_kg = 70))

})
