##################################
## PET Analysis in R            ##
## batch_models.R               ##
## (C) Eric E. Brown  2018      ##
## PEAR v devel                 ##
## Beta version--check all work ##
##################################

# Contains the names and function names of all the models for use in tm_batch
# and model_batch
#'@noRd
model_definitions <- function() {
  return(c(SUVR=calcSUVR, 
  	       eslope=peakSlope, 
  	       max=maxTAC, 
  	       Logan=DVR_all_reference_Logan
  	     ))
}

#' Calculate a model, e.g. SUVR or Logan DVR, for ROIs in a participant batch
#'
#' Use tm_batch instead. For a vector of participant IDs and correspondingly 
#' named tac and volume files, this calculates a model, e.g. SUVR, and stores in 
#' a single table.
#'
#' See calcSUVR() for how SUVR is calculated.
#'
#'@param participants A vector of participant IDs
#'@param model The chosen model e.g. "SUVR"
#'@param file_info A list containing tac and volume file info.
#'@param merge Passes value to calcTAC; T to keep original atomic ROIs
#'@param ROI_def Object that defines combined ROIs, see ROI_definitions.R
#'@param SUVR_def is a vector of the start times for window to be used in SUVR
#'@param ref The name of the reference region for SUVR calculation
#'@param PVC For PVC, set to T where the data is stored as _C in same tac file
#'@param k2prime A fixed value for k2' must be specified (e.g. 0.2)
#'@param t_star If 0, t* will be calculated using find_t_star()
#'@return A table of SUVR values for the specified ROIs for all participants.
#'
model_batch <- function(participants, model, file_info, merge, ROI_def, PVC,
	                    ref=NULL, SUVR_def=NULL, k2prime=NULL, t_star=NULL) {

  # Specify function to use (except Logan, which needs different params) -------
  fn_list <- model_definitions()
  model_fn <- fn_list[[model]]

  # Generate the file names ----------------------------------------------------
  tac_f <- paste0(file_info$dir, participants, file_info$tac_file_suffix)
  vol_f <- paste0(file_info$dir, participants, file_info$vol_file_suffix)
   
  # Empty data.frame to store the calculated values-----------------------------   
  
  tac1 <- loadTACfile(tac_f[1], file_info$tac_format)
  vol1 <- loadVolumes(vol_f[1], format=file_info$vol_format)
  tac_data1 <- calcTAC(tac1, vol1, ROI_def=ROI_def, PVC=PVC, merge=merge)
  master <- as.data.frame(matrix(nrow = length(participants), 
  								 ncol=(length(names(tac_data1))-2) ))
  names(master) <- names(tac_data1)[3:length(names(tac_data1))]
  row.names(master) <- participants

  # Runs through each participant to calculate the model and store the values---
  for (i in 1:length(participants)) {
    message(paste("Working on...", participants[i]))
        
    taci <- loadTACfile(tac_f[i], file_info$tac_format)
    voli <- loadVolumes(vol_f[i], format=file_info$vol_format)
    tac_data <- calcTAC(taci, voli, ROI_def=ROI_def, PVC=PVC, merge=merge)
        
    if (model == "Logan") {
      VALUE <- model_fn(tac_data, ref=ref, k2prime=k2prime, t_star=t_star)
      } else VALUE <- model_fn(tac_data, SUVR_def=SUVR_def, ref=ref) 
    master[participants[i], ] <- t(VALUE)
  }

    return(master)
}
