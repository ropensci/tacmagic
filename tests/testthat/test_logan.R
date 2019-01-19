# test_logan.R

context("Non-invasive Logan graphical method")

test_that("DVR_all_ref_Logan() produces the same results as existing tools", {

  # Prepare tac data ----------------------------------------------------------
  f_raw_tac <- system.file("extdata", "AD06.tac", package="tacmagic")
  f_raw_vol <- system.file("extdata", "AD06_TAC.voistat", package="tacmagic")

  tac <- load_tac(f_raw_tac)
  vol <- load_vol(f_raw_vol)

  AD06_tac_nc <- tac_roi(tac, vol, roi_ham_full(), merge=F, PVC=F)

  # Calculate Logan DVRs with different settings ------------------------------
  nok2_integrate <- DVR_all_ref_Logan(AD06_tac_nc, 
                                      ref="cerebellum", 
                                      k2prime=NULL, 
                                      t_star=24, 
                                      method="integrate")

  k2_integrate <- DVR_all_ref_Logan(AD06_tac_nc, 
                                    ref="cerebellum", 
                                    k2prime=0.2, 
                                    t_star=24, 
                                    method="integrate")

  nok2_trapz <- DVR_all_ref_Logan(AD06_tac_nc, 
                                    ref="cerebellum", 
                                    k2prime=NULL, 
                                    t_star=24, 
                                    method="trapz")

  k2_trapz <- DVR_all_ref_Logan(AD06_tac_nc, 
                                ref="cerebellum", 
                                k2prime=0.2, 
                                t_star=24, 
                                method="trapz")  

  # externally calculated data-------------------------------------------------

  #c("leftfrontal", "rightfrontal", "lefttemporal", "righttemporal", 
  #"leftparietal", "rightparietal", "leftoccipital", "rightoccipital", 
  #"leftcingulate", "rightcingulate", "frontal", "temporal", "parietal", 
  #"occipital", "cingulate", "cerebellum", "totalcortical")

  tpcc_logan_nok2 <- c(1.7315,1.8223,1.7025,1.7985,1.7137,1.7989,1.4318,1.5549,
                     1.9185,1.9504,1.7773,1.7505,1.7567,1.4936,1.9342,1,1.7285)
  
  tpcc_logan_k2 <- c(1.7564,1.8467,1.7251,1.8210,1.7429,1.8282,1.4535,1.5803,
                   1.9468,1.9820,1.8021,1.7731,1.7859,1.5172,1.9642,1,1.7537)

  # tests ---------------------------------------------------------------------
  expect_equal(nok2_integrate[1:17,], tpcc_logan_nok2, tolerance=0.0001)
  expect_equal(nok2_trapz[1:17,], tpcc_logan_nok2, tolerance=0.0001)
  expect_equal(k2_integrate[1:17,], tpcc_logan_k2, tolerance=0.0001)
  expect_equal(k2_trapz[1:17,], tpcc_logan_k2, tolerance=0.0001)

})
