# test_loading.R

context("file loading tests")

test_that("load_voistat() accurately loads and weights model data", {

  vs_f <- system.file("extdata", "AD06_BPnd_BPnd_Logan.voistat", 
  	                  package="tacmagic")
  vs <- load_voistat(vs_f, roi_ham_pib(), model="Logan")
  vs_null <- load_voistat(vs_f, model="Logan")

  
  expect_equal(length(vs[,1]), 109)
  expect_equal(typeof(vs$Logan), "double")
  expect_equal(row.names(vs), c(row.names(vs_null), names(roi_ham_pib())))

  # manually calculated values
  expect_equal(vs["Amygdala_r",], 0.3647066959315658)
  expect_equal(vs_null["Amygdala_r",], 0.3647066959315658)
  expect_equal(vs["lefttemporal",], 0.755352081868971)   

})

test_that("validate_tac() properly rejects malformed tac objects", {

  f_raw_tac <- system.file("extdata", "AD06.tac", package="tacmagic") 
  
  tac <- load_tac(f_raw_tac)
  attributes(tac)$tm_type <- "fail"
  expect_error(validate_tac(tac))

  tac <- load_tac(f_raw_tac)
  attributes(tac)$time_unit <- "sec"
  expect_error(validate_tac(tac))

  tac <- load_tac(f_raw_tac)
  names(tac)[1] <- "mid"
  expect_error(validate_tac(tac))

  tac <- load_tac(f_raw_tac)
  names(tac)[2] <- "mid"
  expect_error(validate_tac(tac))

})	
