##################################
## tacmagic - PET Analysis in R ##
## data.R                       ##
## (C) Eric E. Brown  2018      ##
## Beta version--check all work ##
##################################

#' Fake DVR data for vignette and package testing
#'
#' A fake dataset of 50 simulated participants in the format that the function
#' tm_batch() would be expected to produce with the "Logan" model specified.
#' The data itself was generated as follows:
#' 
#' higher <- matrix(rnorm(40, 1.9, 0.6), ncol=4, nrow=10)
#' lower <- matrix(rnorm(160, 1.3, 0.3), ncol=4, nrow=40)
#' fake_data <- as.data.frame(rbind(higher, lower))
#' row.names(fake_data) <- paste0("p", 1:50)
#' colnames(fake_data) <- c("ROI1_DVR", "ROI2_DVR", "ROI3_DVR", "ROI4_DVR")
#' save(fake_data, "fake_DVR.Rda")
#'
#' @format A data frame with 50 rows and 4 variables representing ROIs
"fake_DVR"
