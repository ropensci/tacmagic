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
