##################################
## tacmagic - PET Analysis in R ##
## tac.R                        ##
## (C) Eric E. Brown  2018      ##
## Beta version--check all work ##
##################################

#' Calculate weighted time-activity curves for specified regions of interest
#'
#'@export
#'@param tac The time-activity curve data from loading function
#'@param volumes The ROI volume data from loading function
#'@param ROI_def The definition of ROIs by combining smaller ROIs from TAC file
#'@param merge If TRUE, includes the original ROIs in the output data
#'@param PVC If TRUE, appends "_C" to ROI name header (as in PMOD TAC files)
#'@family tac functions 
#'@return Time-activity curves for the specified ROIs
#'@examples 
#' # f_raw_tac and f_raw_vol are the filenames of PMOD-generated files
#' f_raw_tac <- system.file("extdata", "AD06.tac", package="tacmagic") 
#' f_raw_vol <- system.file("extdata", "AD06_TAC.voistat", package="tacmagic")
#' 
#' tac <- load_tac(f_raw_tac)
#' vol <- load_vol(f_raw_vol)
#' AD06_tac_nc <- tac_roi(tac, vol, roi_ham_full(), merge=FALSE, PVC=FALSE)
tac_roi <- function(tac, volumes, ROI_def, merge, PVC) {
    
  if(!validate_tac(tac)) stop("Supplied tac file did not validate.")

  ROI_PVC <- ROI_def
    
  if (PVC) {
      for (i in seq_along(ROI_PVC)) {
        ROI_PVC[i] <- lapply(ROI_PVC[i], paste, "_C", sep="")
      }
  }
    
  # Setup the output data.frame
  m <- matrix(nrow=length(tac[,1]), ncol=length(ROI_def))
  calculated_TACs <- as.data.frame(m)
  names(calculated_TACs) <- names(ROI_def)

  # Calculate the weighted mean TACs for each ROI in the definition list
  for (i in seq_along(ROI_def)) {
      calculated_TACs[i] <- apply(tac[,ROI_PVC[[i]]], 1, weighted.mean,
                                  volumes[ROI_def[[i]],])
  }
    
  if (merge) {
    calculated_TACs <- data.frame(tac, calculated_TACs)
  } else {
      calculated_TACs <- data.frame(tac[1:2], calculated_TACs)
  }
  
  attributes(calculated_TACs) <- copy_tac_attributes(tac, calculated_TACs)

  if(!validate_tac(calculated_TACs)) stop("Merged ROI tac file did not 
                                           validate.")

  return(calculated_TACs)
}
