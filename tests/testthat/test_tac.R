# test_tac.R

context("TAC data loading and weighted averages")

test_that("calcTAC accurately calculates weighted averages from PMOD .tac and .voistat files", {
  
  file_ans_nc <- system.file("extdata", "AD06_man_fullROI.csv", package="tacmagic")
  file_ans_pvc <- system.file("extdata", "AD06_man_fullROI_c.csv", package="tacmagic")
  file_raw_tac <- system.file("extdata", "AD06.tac", package="tacmagic")
  file_raw_vol <- system.file("extdata", "AD06_TAC.voistat", package="tacmagic")

  AD06_tac_nc <- calcTAC(loadTACfile(file_raw_tac), loadVolumes(file_raw_vol), fullROIs(), merge=F, PVC=F)
  ans_nc <- read.csv(system.file("extdata", "AD06_man_fullROI.csv", package="tacmagic"))

  expect_equal(AD06_tac_nc, ans_nc)

  AD06_tac_pvc <- calcTAC(loadTACfile(file_raw_tac), loadVolumes(file_raw_vol), fullROIs(), merge=F, PVC=T)
  ans_pvc <- read.csv(system.file("extdata", "AD06_man_fullROI_c.csv", package="tacmagic"))

  expect_equal(AD06_tac_pvc, ans_pvc)   

})