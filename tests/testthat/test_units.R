# test_units.R

context("unit conversion")

test_that("change_units() converts numerics accurately", {

  expect_equal(0.185, change_units(5, to_unit = "kBq", from_unit = "nCi"))
  expect_equal(5, change_units(0.185, to_unit = "nCi", from_unit = "kBq"))
  expect_equal(111, change_units(3, to_unit = "MBq", from_unit = "mCi"))
  expect_equal(3, change_units(111, to_unit = "mCi/cc", from_unit = "MBq/cc"))

})

test_that("change_units() converts tac objects accurately", {

  f <- system.file("extdata", "AD06.tac", package="tacmagic")
  AD06_tac <- load_tac(f, format="PMOD")
  AD06_tac_nCicc <- change_units(AD06_tac, to_unit = "nCi/cc")

  expect_equivalent(AD06_tac[10,10], 18.52552, tolerance = 0.00001)
  expect_equivalent(change_units(18.52552, from_unit="kBq/cc",
                                 to_unit= "nCi/cc"),
                    AD06_tac_nCicc[10,10], tolerance = 0.00001)
})

