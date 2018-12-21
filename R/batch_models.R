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

  # Specify the function to use (except Logan, which needs different params)
	
  if (model == "SUVR") {
    model_function <- calcSUVR
  } else if (model == "eslope") { 
	  model_function <- peakSlope
	  } else if (model == "max") {
		  model_function <- maxTAC
	    } else if (model == "Logan") {
	    	message("Using the Logan model.")
	      } else {
	    	  error("An invalid mode was specified for batchTACvalue().")
	        }

  # Generate the file names
  tac_files <- paste(file_info$dir, participants, file_info$tac_file_suffix, 
    	               sep="")
  vol_files <- paste(file_info$dir, participants, file_info$vol_file_suffix, 
    				   sep="")
    
  #Set up the output file by using the first participant as a template.
  vol1 <- loadVolumes(vol_files[1], format=file_info$vol_format)
  message("Loading first tac to create template table.")
  tac1 <- loadTACfile(tac_files[1], file_info$tac_format)
  first_tac <- calcTAC(tac1, volumes=vol1, ROI_def=ROI_def, PVC=PVC,
  	                   merge=merge)
  message(paste("Loaded first tac file. Calculating", model))
    
  if (model == "Logan") {
    first <- DVR_all_reference_Logan(first_tac, reference=reference,
    						               k2prime=k2prime, t_star=t_star)
   } else {
       first <- model_function(first_tac, 
         	                   SUVR_def=SUVR_def, reference=reference) 
     }
    		
  message("First participant calculated.")
  master <- t(first)[-1,]
  message("Empty master table complete; iterating through all participants.")
    
  # Runs through each participant to calculate the model and store the values.
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
      row.names(trans) <- participants[i]
      master <- rbind(master,trans)
    }
 
    return(as.data.frame(master))
}
