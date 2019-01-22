##################################
## tacmagic - PET Analysis in R ##
## batch_models.R               ##
## (C) Eric E. Brown  2018      ##
## Beta version--check all work ##
##################################

#' List of models and their names available to tm_batch()
#'
#' Contains the names and function names of all the models for use in tm_batch
#'
#' @return list of functions available to tm_batch.
#' @noRd
model_definitions <- function() {
  return(c(SUVR=suvr, 
           Logan=DVR_all_ref_Logan
         ))
}

#' Calculate a model, e.g. SUVR or Logan DVR, for ROIs in a participant batch
#'
#' Use tm_batch instead. For a vector of participant IDs and correspondingly 
#' named tac and volume files, this calculates a model, e.g. SUVR, and stores 
#' in a single table.
#'
#' See suvr() for how SUVR is calculated.
#'
#'@param all_tacs A list of tac data.frames (e.g. from load_batch())
#'@param model Either a character string representing an available model, 
#'             e.g."SUVR", or a custom function passed to it from batch_tm
#'@param params Parameters passed from batch_tm()
#'@return A data.frame of SUVR values for the ROIs for all participants
#'@noRd
model_batch <- function(all_tacs, model=NULL, params) {

  # Specify function to use (except Logan, which needs different params) ------
  if (class(model) == "function") {
    model_fn <- model
  } else if (class(model)=="character") {
      fn_list <- model_definitions()
      model_fn <- fn_list[[model]]
  }

  participants <- names(all_tacs)

  # Empty data.frame to store the calculated values----------------------------
  tac_data1 <- all_tacs[[1]]
  master <- as.data.frame(matrix(nrow = length(participants), 
                          ncol=(length(names(tac_data1))-2) ))
  names(master) <- names(tac_data1)[3:length(names(tac_data1))]
  row.names(master) <- participants

  # Runs through each participant to calculate the model and store the values--
  for (i in seq_along(participants)) {
    message(paste("Working on...", participants[i]))
        
    tac_data <- all_tacs[[i]]    
    VALUE <- model_fn(tac_data, params=params)
    master[participants[i], ] <- t(VALUE)
  }

  return(master)
}

#' Used by the user-facing function batch_load()
#'
#' Takes a participant ID, and what is needed to make the file names, and loads
#' the tac/vol files, then does ROI merging as specified; returns a list of tac
#' information, each element is a participant.
#'@noRd
load_tacs <- function(participant, roi_m, dir, tac_format, tac_file_suffix, 
                      vol_file_suffix=NULL, vol_format=NULL, ROI_def=NULL, 
                      PVC=NULL, merge=NULL) {
  
  tac_f <- paste0(dir, participant, tac_file_suffix) 
  tac <- load_tac(tac_f, format=tac_format)

  if (roi_m) {
    vol_f <- paste0(dir, participant, vol_file_suffix)
    vol <- load_vol(vol_f, format=vol_format)
    out <- tac_roi(tac, vol, ROI_def=ROI_def, PVC=PVC, merge=merge)
  } else out <- tac

  return(out)
}
