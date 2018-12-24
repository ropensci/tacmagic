##################################
## PET Analysis in R            ##
## batch_models.R               ##
## (C) Eric E. Brown  2018      ##
## PEAR v devel                 ##
## Beta version--check all work ##
##################################


#' Calculate a model, e.g. SUVR or Logan DVR, for ROIs in a participant batch
#'
#' Use tm_batch instead. For a vector of participant IDs and correspondingly 
#' named tac and volume files, this calculates a model, e.g. SUVR, and stores in 
#' a single table.
#'
#' See calcSUVR() for how SUVR is calculated.
#'
#'@export
#'@param participants A vector of participant IDs
#'@param model The chosen model e.g. "SUVR"
#'@param dir A directory and/or file name prefix for the tac/volume files
#'@param file_info A list containing tac and volume file info.
#'@param ROI_def Object that defines combined ROIs, see ROI_definitions.R
#'@param SUVR_def is a vector of the start times for window to be used in SUVR
#'@param reference The name of the reference region for SUVR calculation
#'@param PVC For PVC, set to T where the data is stored as _C in same tac file
#'@return A table of SUVR values for the specified ROIs for all participants.
#'@examples
#'
model_batch <- function(participants, model, file_info, merge, ROI_def, PVC,
	                    reference=NULL, SUVR_def=NULL, k2prime=NULL, 
	                    t_star=NULL) {

  # Specify function to use (except Logan, which needs different params) -------
  if (model != "Logan") {
  	model_function <- set_model(model)
  } else message("Using the Logan model.")


  # Generate the file names ----------------------------------------------------
  tac_files <- paste(file_info$dir, participants, file_info$tac_file_suffix, 
    	               sep="")
  vol_files <- paste(file_info$dir, participants, file_info$vol_file_suffix, 
    				   sep="")
   
  # Empty data.frame to store the calculated values-----------------------------   
  master <- as.data.frame(matrix(nrow = length(participants), 
  								 ncol=length(names(ROI_def)) ) )
  names(master) <- names(ROI_def)
  row.names(master) <- participants

  # Runs through each participant to calculate the model and store the values---
  for (i in 1:length(participants)) {
      message(paste("Working on...", participants[i]))
        
      taci <- loadTACfile(tac_files[i], file_info$tac_format)
      voli <- loadVolumes(vol_files[i], format=file_info$vol_format)
      tac_data <- calcTAC(taci, voli, ROI_def=ROI_def, PVC=PVC, merge=merge)
        
      if (model == "Logan") {
        VALUE <- DVR_all_reference_Logan(tac_data, reference=reference,
        					             k2prime=k2prime, t_star=t_star)
        } else {
          VALUE <- model_function(tac_data, 
                                  SUVR_def=SUVR_def, reference=reference) 
          }
      trans <- t(VALUE)
      master[participants[i], ] <- trans
    }
 
    return(as.data.frame(master))
}

set_model <- function(model_name) {

  if (model_name == "SUVR") {
    model_function <- calcSUVR
  } else if (model_name == "eslope") { 
	  model_function <- peakSlope
	  } else if (model_name == "max") {
		  model_function <- maxTAC
	    } else {
	    	  error("An invalid mode was specified for batchTACvalue().")
	        }
  return(model_function)
}

