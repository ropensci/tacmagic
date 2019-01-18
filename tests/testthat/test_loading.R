# test_loading.R

context(".voistat file loading")

test_that("load_voistat() accurately loads and weights model data", {

  vs_f <- system.file("extdata", "AD06_BPnd_BPnd_Logan.voistat", package="tacmagic")
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
