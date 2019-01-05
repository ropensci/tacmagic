# test_tac.R

context("TAC data loading and weighted averages")

test_that("tac_roi() accurately calculates weighted averages from PMOD .tac and 
	      .voistat files", {
  
  f_ans_nc <- system.file("extdata", "AD06_man_fullROI.csv", package="tacmagic")
  f_ans_pvc <- system.file("extdata", "AD06_man_fullROI_c.csv", 
  	                       package="tacmagic")
  f_raw_tac <- system.file("extdata", "AD06.tac", package="tacmagic")
  f_raw_vol <- system.file("extdata", "AD06_TAC.voistat", package="tacmagic")

  ans_nc <- read.csv(f_ans_nc)
  ans_pvc <- read.csv(f_ans_pvc)

  tac <- load_tac(f_raw_tac)
  vol <- load_vol(f_raw_vol)

  AD06_tac_nc <- tac_roi(tac, vol, fullROIs(), merge=F, PVC=F)
  AD06_tac_pvc <- tac_roi(tac, vol, fullROIs(), merge=F, PVC=T)
  
  expect_equal(AD06_tac_nc, ans_nc)
  expect_equal(AD06_tac_pvc, ans_pvc)

})

test_that("tac_roi() accurately calculates weighted averages from PMOD .voistat 
	      and .acqtimes files", {
  
  f_ans_nc <- system.file("extdata", "AD06_man_fullROI.csv", package="tacmagic")
  f_ans_pvc <- system.file("extdata", "AD06_man_fullROI_c.csv", 
  	                       package="tacmagic")
  f_acq <- system.file("extdata", "AD06.acqtimes", package="tacmagic")
  f_voistat <- system.file("extdata", "AD06_TAC.voistat", package="tacmagic")

  tac <- load_tac(f_voistat, format="voistat", acqtimes=f_acq)
  vol <- load_vol(f_voistat)

  AD06_tac_nc_vs <- tac_roi(tac, vol, fullROIs(), merge=F, PVC=F)
  AD06_tac_pvc_vs <- tac_roi(tac, vol, fullROIs(), merge=F, PVC=T)

  ans_nc <- read.csv(f_ans_nc)
  ans_pvc <- read.csv(f_ans_pvc)

  expect_equal(AD06_tac_nc_vs, ans_nc)
  expect_equal(AD06_tac_pvc_vs, ans_pvc)

})