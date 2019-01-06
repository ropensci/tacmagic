##################################
## tacmagic - PET Analysis in R ##
## SUVR.R                       ##
## (C) Eric E. Brown  2018      ##
## Beta version--check all work ##
##################################

#' Calculate weighted SUVRs for specified regions of interest
#'
#' Calculate the standardized uptake value ratio (SUVR) for all ROIs in the
#' provided tac data, using the specified reference region.
#'
#'@export 
#'@param tac The time-activity curve data from tac_roi()
#'@param SUVR_def a vector of start times for window to be used in SUVR
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

#' Calculate SUVRs for regions of interest with AUC from mid-frame times
#'
#' Calculate the standardized uptake value ratio (SUVR) for all ROIs in the
#' provided tac data, using the specified reference region. This is an alternate
#' to suvr() which should provide very similar values.
#'
#'@export 
#'@param tac The time-activity curve data from tac_roi()
#'@param SUVR_def a vector of start times for window to be used in SUVR
#'@param reference is a string, e.g. "cerbellum", to specify reference region
#'@return A data.frame of SUVR values for the specified ROIs
suvr_auc <- function(tac, SUVR_def, reference) {

    SUVRtable <- new_table(tac, "SUVR")
    
    tac$mid <- (tac$start + tac$end) / 2

    for (ROI in names(tac)[-c(1:2, length(tac))]) {    
        rich <- pracma::trapz(tac[(tac$start %in% SUVR_def),][,"mid"], 
                              tac[(tac$start %in% SUVR_def),][,ROI])
        poor <- pracma::trapz(tac[(tac$start %in% SUVR_def),][,"mid"], 
                              tac[(tac$start %in% SUVR_def),][,reference])
        SUVRtable[ROI, "SUVR"] <-  rich/poor
    }    

    return(SUVRtable)
}