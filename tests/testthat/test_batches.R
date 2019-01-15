# test_batches.R

context("batch_tm produces expected results")

test_that("tac_roi() accurately calculates weighted averages from PMOD .tac and 
	      .voistat files", {
  
  
  f_raw_tac <- system.file("extdata", "AD06.tac", package="tacmagic")
  f_raw_vol <- system.file("extdata", "AD06_TAC.voistat", package="tacmagic")
  tac <- load_tac(f_raw_tac)
  vol <- load_vol(f_raw_vol)
  AD06_tac_pvc <- tac_roi(tac, vol, roi_ham_full(), merge=F, PVC=T)

  Fake1 <- AD06_tac_pvc
  Fake2 <- AD06_tac_pvc
  Fake1[1:34, c(3:17, 19:24)] <- Fake1[1:34, c(3:17, 19:24)] + 1
  Fake2[1:34, c(3:17, 19:24)] <- Fake2[1:34, c(3:17, 19:24)] + 2

  batch_sim <- list("Fake1"=Fake1, "AD06"=AD06_tac_pvc, "Fake2"=Fake2)

  models_to_run <- c("SUVR", "Logan")
  batch_result <- batch_tm(batch_sim, 
  	                       models=models_to_run, 
  	                       SUVR_def=c(3000, 3300, 3600),
  	                       ref="cerebellum",
  	                       k2prime=0.2,
  	                       t_star=24)

  expect_equal(length(roi_ham_full()) * length(models_to_run), length(batch_result))
  expect_equal(3, nrow(batch_result))

  expect_equal(as.numeric(unlist(suvr(Fake1, SUVR_def=c(3000, 3300, 3600),ref="cerebellum"))), as.numeric(unlist(t(batch_result[1,1:22]))))
  expect_equal(as.numeric(unlist(suvr(AD06_tac_pvc, SUVR_def=c(3000, 3300, 3600),ref="cerebellum"))), as.numeric(unlist(t(batch_result[2,1:22]))))

})

