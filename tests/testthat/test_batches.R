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

  expect_error(batch_tm(batch_sim, models=c("SUVR", "not"), 
                        SUVR_def=c(3000, 3300, 3600), ref="cerebellum",
                        k2prime=0.2, t_star=24))

  expect_equal(length(roi_ham_full()) * length(models_to_run), 
               length(batch_result))
  expect_equal(3, nrow(batch_result))

  expect_equal(as.numeric(unlist(suvr(Fake1, SUVR_def=c(3000, 3300, 3600),
                                      ref="cerebellum"))), 
               as.numeric(unlist(t(batch_result[1,1:22]))))
  expect_equal(as.numeric(unlist(suvr(AD06_tac_pvc, 
                                      SUVR_def=c(3000, 3300, 3600),
                                      ref="cerebellum"))), 
               as.numeric(unlist(t(batch_result[2,1:22]))))

  models_to_run2 <- c("Logan", "SUVR")   # switch order
  tmp <- tempfile()
  batch_result2 <- batch_tm(batch_sim, 
                           models=models_to_run2, 
                           SUVR_def=c(3000, 3300, 3600),
                           ref="cerebellum",
                           k2prime=0.2,
                           t_star=24,
                           master=batch_result,
                           outfile=tmp)

  # SUVR and DVR results appear twice and same as from original run
  expect_equal(as.numeric(unlist(batch_result2[,1:22])),
               as.numeric(unlist(batch_result2[,67:88])))
  expect_equal(as.numeric(unlist(batch_result2[,23:44])),
               as.numeric(unlist(batch_result2[,45:66])))


})

test_that("batch_voistat() loads 3 participants and produces same result as 
           individual load_voistat()", {
  

  participants <- c(system.file("extdata", "AD06_BPnd_BPnd_Logan.voistat", 
                                 package="tacmagic"),
                    system.file("extdata", "AD07_BPnd_BPnd_Logan.voistat", 
                                 package="tacmagic"),
                    system.file("extdata", "AD08_BPnd_BPnd_Logan.voistat", 
                                 package="tacmagic"))

  batchtest <- batch_voistat(participants=participants, ROI_def=roi_ham_pib(), 
                             dir="", filesuffix="", varname="Logan", 
                             otherdata=NULL, outfile=NULL) 

  vs_f <- system.file("extdata", "AD06_BPnd_BPnd_Logan.voistat", 
                      package="tacmagic")
  vs <- load_voistat(vs_f, roi_ham_pib(), model="Logan")

  expect_equal(as.numeric(unlist(vs)), as.numeric(unlist(batchtest[1,])))

 
  # ensure batch_voistat can add to data from batch_tm()

  f_raw_tac <- system.file("extdata", "AD06.tac", package="tacmagic")
  f_raw_vol <- system.file("extdata", "AD06_TAC.voistat", package="tacmagic")
  f_raw_tac2 <- system.file("extdata", "AD07.tac", package="tacmagic")
  f_raw_vol2 <- system.file("extdata", "AD07_TAC.voistat", package="tacmagic")
  tac <- load_tac(f_raw_tac)
  vol <- load_vol(f_raw_vol)
  tac2 <- load_tac(f_raw_tac2)
  vol2 <- load_vol(f_raw_vol2)
  AD06 <- tac_roi(tac, vol, roi_ham_full(), merge=F, PVC=T)
  AD07 <- tac_roi(tac2, vol2, roi_ham_full(), merge=F, PVC=T)
  batch <- list(a=AD06, b=AD07)
  names(batch) <- participants[1:2]
  
  batchSUVR <- batch_tm(batch, ref="cerebellum",
                           models="SUVR", SUVR_def=c(3000, 3300, 3600))

  tmp <- tempfile()  # make sure accepts a file to write to
  batchtest2 <- batch_voistat(participants=participants[1:2], 
                              ROI_def=roi_ham_pib(),
                              dir="", filesuffix="", varname="Logan",
                              otherdata=batchSUVR, outfile=tmp)

  expect_equal(batchSUVR[1,1], batchtest2[1,1])
  expect_equal(batchtest[1,1], batchtest2[1,23])
})

test_that("batch_load() loads 3 particpants and produces same result as 
           individual load_tac()", {

  participants <- c(system.file("extdata", "AD06.tac", package="tacmagic"),
                    system.file("extdata", "AD07.tac", package="tacmagic"),
                    system.file("extdata", "AD08.tac", package="tacmagic"))

  batchtest <- batch_load(participants, tac_file_suffix="")

  AD06 <- load_tac(system.file("extdata", "AD06.tac", package="tacmagic"))
  AD07 <- load_tac(system.file("extdata", "AD07.tac", package="tacmagic"))
  AD08  <- load_tac(system.file("extdata", "AD08.tac", package="tacmagic"))

  expect_identical(names(batchtest), participants)
  expect_identical(AD06, batchtest[[1]])
  expect_identical(AD07, batchtest[[2]])
  expect_identical(AD08, batchtest[[3]])

})

test_that("batch_load() with merging loads 3 particpants with same result as 
           individual load_tac() and tac_roi()", {

  participants <- c(system.file("extdata", "AD06.tac", package="tacmagic"),
                    system.file("extdata", "AD07.tac", package="tacmagic"),
                    system.file("extdata", "AD08.tac", package="tacmagic"))

  participants <- strtrim(participants, nchar(participants) - 4)

  batchtest2 <- batch_load(participants, tac_file_suffix=".tac", roi_m=TRUE,
                           vol_file_suffix="_TAC.voistat", 
                           vol_format="voistat", merge=TRUE, 
                           ROI_def=roi_ham_stand())
  
  AD07 <- load_tac(system.file("extdata", "AD07.tac", package="tacmagic"))
  AD07_vol <- load_vol(system.file("extdata", "AD07_TAC.voistat", 
                       package="tacmagic"))
  AD07_m <- tac_roi(AD07, AD07_vol, roi_ham_stand(), PVC=FALSE, merge=TRUE)

  expect_identical(AD07_m, batchtest2[[2]])

})

#test_that("batch_tm() gives error when unavailable model is specified.", {


#})
