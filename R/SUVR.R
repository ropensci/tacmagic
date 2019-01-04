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
#'@export 
#'@param tac The time-activity curve data from calcTAC()
#'@param SUVR_def is a vector of the start times for window to be used in SUVR
#'@param reference is a string, e.g. "cerbellum", to specify reference region
#'@return A data.frame of SUVR values for the specified ROIs
suvr <- function(tac, SUVR_def, reference) {

    SUVRtable <- new_table(tac, "SUVR")
    
    # TODO validate that t1$start and t2$end are numeric
    frames <- match(SUVR_def, tac$start)
    frame_weights <- tac$end[frames] - tac$start[frames]
    
    for (ROI in names(tac)[-(1:2)]) {
        rich <- weighted.mean(tac[frames,][,ROI], frame_weights)
        poor <- weighted.mean(tac[frames,][,reference], frame_weights)
        SUVRtable[ROI, "SUVR"] <-  rich/poor
    }
    return(SUVRtable)
}

