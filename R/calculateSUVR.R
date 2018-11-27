##################################
## PET Analysis in R            ##
## calculateSUVR.R              ##
## (C) Eric E. Brown  2018      ##
## PEAR v devel                 ##
## Beta version--check all work ##
##################################

#' Calculate weighted SUVRs for specified regions of interest
#'
#' When smaller ROIs need to be combined into larger ROIs, e.g. when TACs have
#' been calculated for components of the frontal lobe, but an SUVR is desired
#' for the entire frontal lobe, the TACs need to be combined, and the relative
#' volumes of each needs to be taken into account. This function calculates 
#' those weighted means.
#'
#'@param tac The time-activity curve data from loading function.
#'@param volumes The ROI volume data from loading function
#'@param SUVR_def is a vector of the start times for window to be used in SUVR
#'@param corrected For PVC, true where the data is stored as _C in same tac file
#'@return A table of SUVR values for the specified ROIs
#'@examples 
#' calcSUVR(p1tac, p1vol, standardROIs(), c("3000", "3300", "3600", "3900"))
legacy_calcSUVR <- function(tac, volumes, ROI_def, SUVR_def, corrected=TRUE) {

  if (FALSE == verify_window_durations(tac, SUVR_def)) {
    warning("Frames durations are of unequal length.")
  }

  tac <- data.frame(tac, row.names=1)
  proportiontable <- calcRelativeVolumes(volumes, ROI_def)
  denominator <- length(SUVR_def)

  # Creates a data.frame to store the means (over the SUVR window) and relative 
  # volumes of each ROI
  means <- mean_table(ROI_def)

  # This fills in the mean table. Note the _C is added to get the PVC-corrected 
  # values by default, unless corrected=FALSE
  for (subROI in ROI_def@all) {
    single_mean <- sum(tac[SUVR_def, correct(corrected, subROI)])/denominator
    means <- fill_means_table(single_mean, subROI, means, proportiontable)
  }

  # Data frame to store the calculated SUVRs, which will be returned.
  SUVRtable <- create_final_table(ROI_def, "SUVR")

  # This step calculates the SUVR for each hemilobe by iterating through each 
  # ROI name (from hemilobe names) and ROI in ROI_def@hemilobe. This speaks to 
  # the critical importance of both sources having the same order, so be 
  # cautious if changing the standardROIs() function.

  SUVRtable <- weighted_average(ROI_def@hemilobe, names(ROI_def@hemilobe),
    means, SUVRtable, "SUVR", "proportion_of_hemilobe")
  SUVRtable <- weighted_average(ROI_def@lobe, names(ROI_def@lobe), means,
    SUVRtable, "SUVR", "proportion_of_lobe")
  SUVRtable <- weighted_average(ROI_def@totalcortical, "totalcortical", 
    means, SUVRtable, "SUVR", "proportion_of_total")
  # Gets the cerebellum value to use as reference and calculate SUVR with
  cerebellumreference <- (means["Cerebellum_l", "mean"] * 
                          means["Cerebellum_l", "proportion_of_lobe"]) + 
                         (means["Cerebellum_r", "mean"] * 
                          means["Cerebellum_r", "proportion_of_lobe"])
  SUVRtable <- SUVRtable/cerebellumreference

  return(SUVRtable)
}

#' Calculate weighted SUVRs for specified regions of interest
#'
#' When smaller ROIs need to be combined into larger ROIs, e.g. when TACs have
#' been calculated for components of the frontal lobe, but an SUVR is desired
#' for the entire frontal lobe, the TACs need to be combined, and the relative
#' volumes of each needs to be taken into account. This function calculates
#' those weighted means. It also takes into account potentially different
#' frame durations.
#'
#'@param tac The time-activity curve data from loading function
#'@param volumes The ROI volume data from loading function
#'@param SUVR_def is a vector of the start times for window to be used in SUVR
#'@return A data.frame of SUVR values for the specified ROIs
#'@examples
#' calcSUVR(p1tac, p1vol, standardROIs(), c("3000", "3300", "3600", "3900"))
calcSUVR <- function(tac, volumes, ROI_def, SUVR_def, reference="cerebellum") {
    
    SUVR <- rep(NA, length(ROI_def))
    SUVRtable <- data.frame(row.names=names(ROI_def), SUVR)
    
    # will need to validate that t1$start and t2$end are numeric
    frames <- match(SUVR_def, tac$start)
    frame_weights <- tac$end[frames] - tac$start[frames]
    
    weighted_tacs <- calcTAC(tac, volumes, ROI_def)
    
    for (ROI in names(ROI_def)) {
        rich <- weighted.mean(weighted_tacs[frames,][,ROI], frame_weights)
        poor <- weighted.mean(weighted_tacs[frames,][,reference], frame_weights)
        SUVRtable[ROI, "SUVR"] <-  rich/poor
    }
    
    return(SUVRtable)
    
}
