# test_tac.R

context("TAC data loading and weighted averages")

test_that("tac_roi() accurately calculates weighted averages from PMOD .tac and 
	      .voistat files", {
  
  f_ans_nc <- system.file("extdata", "AD06_man_fullROI.csv", 
                          package="tacmagic")
  f_ans_pvc <- system.file("extdata", "AD06_man_fullROI_c.csv", 
  	                       package="tacmagic")
  f_raw_tac <- system.file("extdata", "AD06.tac", package="tacmagic")
  f_raw_vol <- system.file("extdata", "AD06_TAC.voistat", package="tacmagic")

  ans_nc <- read.csv(f_ans_nc)
  attributes(ans_nc)$time_unit <- "seconds"
  attributes(ans_nc)$activity_unit <- "kBq/cc"
  attributes(ans_nc)$tm_type <- "tac"
  ans_pvc <- read.csv(f_ans_pvc)
  attributes(ans_pvc)$time_unit <- "seconds"
  attributes(ans_pvc)$activity_unit <- "kBq/cc"
  attributes(ans_pvc)$tm_type <- "tac"

  tac <- load_tac(f_raw_tac)
  vol <- load_vol(f_raw_vol)

  AD06_tac_nc <- tac_roi(tac, vol, roi_ham_full(), merge=F, PVC=F)
  AD06_tac_pvc <- tac_roi(tac, vol, roi_ham_full(), merge=F, PVC=T)
  AD06_merge <- tac_roi(tac, vol, roi_ham_full(), merge=T, PVC=F)

  expect_equal(AD06_tac_nc, ans_nc)
  expect_equal(AD06_tac_pvc, ans_pvc)
  expect_equal(validate_tac(AD06_tac_nc), TRUE)
  expect_equal(validate_tac(AD06_tac_pvc), TRUE)
  expect_equal(length(names(AD06_merge)), 196)

})

test_that("tac_roi() accurately calculates weighted averages from PMOD .voistat 
	      and .acqtimes files", {
  
  f_ans_nc <- system.file("extdata", "AD06_man_fullROI.csv", 
                          package="tacmagic")
  f_ans_pvc <- system.file("extdata", "AD06_man_fullROI_c.csv", 
  	                       package="tacmagic")
  f_acq <- system.file("extdata", "AD06.acqtimes", package="tacmagic")
  f_voistat <- system.file("extdata", "AD06_TAC.voistat", package="tacmagic")

  tac <- load_tac(f_voistat, format="voistat", acqtimes=f_acq)
  vol <- load_vol(f_voistat)

  AD06_tac_nc_vs <- tac_roi(tac, vol, roi_ham_full(), merge=F, PVC=F)
  AD06_tac_pvc_vs <- tac_roi(tac, vol, roi_ham_full(), merge=F, PVC=T)

  ans_nc <- read.csv(f_ans_nc)
  attributes(ans_nc)$time_unit <- "seconds"
  attributes(ans_nc)$activity_unit <- "kBq/cc"
  attributes(ans_nc)$tm_type <- "tac"
  ans_pvc <- read.csv(f_ans_pvc)
  attributes(ans_pvc)$time_unit <- "seconds"
  attributes(ans_pvc)$activity_unit <- "kBq/cc"
  attributes(ans_pvc)$tm_type <- "tac"

  expect_equal(AD06_tac_nc_vs, ans_nc)
  expect_equal(AD06_tac_pvc_vs, ans_pvc)
  expect_equal(validate_tac(AD06_tac_nc_vs), TRUE)
  expect_equal(validate_tac(AD06_tac_pvc_vs), TRUE)

})

test_that("tac_roi() can load magia matlab files to the proper format", {

  f_magia <- system.file("extdata", "AD06_tac_magia.mat", package="tacmagic")
  m <- load_tac_magia(f_magia)
  attributes(m)$time_unit <- "seconds"
  attributes(m)$activity_unit <- "kBq/cc"
  attributes(m)$tm_type <- "tac"
  n <- load_tac(f_magia, format="magia", time_unit="seconds", 
  	            activity_unit="kBq/cc")
  expect_is(m, "data.frame")
  expect_equal(names(m)[1:2], c("start", "end"))
  expect_identical(m, n)
  expect_equal(validate_tac(n), TRUE)

})	

test_that("validate_tac() successfully rejects bad files", {

  # good files are tested in tests above

  f_ans_nc <- system.file("extdata", "AD06_man_fullROI.csv", 
                          package="tacmagic")
  ans_nc <- read.csv(f_ans_nc)
  
  expect_error(validate_tac(ans_nc))

  attributes(ans_nc)$time_unit <- "sec"
  attributes(ans_nc)$activity_unit <- "kBq/cc"
  attributes(ans_nc)$tm_type <- "tac"

  expect_error(validate_tac(ans_nc))

  ans_nc <- read.csv(f_ans_nc)
  ans_nc <- ans_nc[,-(1:2)]
  attributes(ans_nc)$time_unit <- "seconds"
  attributes(ans_nc)$activity_unit <- "kBq/cc"
  attributes(ans_nc)$tm_type <- "tac"

  expect_error(validate_tac(ans_nc))

})

test_that("plot_tac runs without error and contains correct axis label", {


  f_raw_tac <- system.file("extdata", "AD06.tac", package="tacmagic") 
  f_raw_vol <- system.file("extdata", "AD06_TAC.voistat", package="tacmagic")
 
  tac <- load_tac(f_raw_tac)
  vol <- load_vol(f_raw_vol)
  AD06_tac_nc <- tac_roi(tac, vol, roi_ham_full(), merge=FALSE, PVC=FALSE)

  pdf(NULL)
  on.exit(dev.off())
  dev.control(displaylist="enable")

  plot_tac(AD06_tac_nc, ROIs=c("frontal", "cerebellum"), title="Example Plot")

  p <- recordPlot()
  
  expect_equal(unlist(p)[[123]], "Time (minutes)")

})
