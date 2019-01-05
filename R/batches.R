##################################
## tacmagic - PET Analysis in R ##
## bathces.R                    ##
## (C) Eric E. Brown  2018      ##
## Beta version--check all work ##
##################################

#' Calculate one or more models for a batch of participants
#'
#' For a vector of participant IDs and correspondingly named tac and volume
#' files, this produces SUVR, DVR, and upslope in a tidy data.frame. Current
#' model options are "SUVR", "Logan" and "eslope".
#'
#' For further details about how the models are calculated, see the indiviudal
#' functions that they rely on. "SUVR" uses suvr(), "Logan" uses
#' DVR_all_ref_Logan(), and "eslope" uses peaksSlope().
#'
#'@export
#'@param participants A vector of participant IDs
#'@param models A vector of names of the models to calculate
#'@param dir A directory and/or file name prefix for the tac/volume files
#'@param tac_format Format of tac files provided: See load_tac()
#'@param tac_file_suffix How participant IDs corresponds to the TAC files
#'@param vol_format The file format that includes volumes: See load_vol()
#'@param vol_file_suffix How participant IDs correspond to volume files
#'@param ROI_def Object that defines combined ROIs, see ROI_definitions.R
#'@param SUVR_def is a vector of the start times for window to be used in SUVR
#'@param PVC For PVC, true where the data is stored as _C in same tac file
#'@param ref The name of the reference region for DVR/SUVR calculation
#'@param merge Passes value to tac_roi(); T to keep original atomic ROIs
#'@param k2prime Fixed k2' for DVR calculation
#'@param t_star Change from 0 to manually specify a t* for DVR calculation
#'@param master Optionally, a data.frame of same format as return, to add to
#'@param outfile Specify a filename to save the data
#'@return A table of SUVR values for the specified ROIs for all participants
#'
tm_batch <- function(participants, models=c("SUVR", "Logan", "eslope"), PVC=F,
                     dir="", tac_format="PMOD", tac_file_suffix=".tac", 
                     vol_file_suffix="_TAC.voistat", vol_format="Voistat", 
                     ROI_def=NULL, SUVR_def=NULL, ref="cerebellum", merge=F, 
                     k2prime=NULL, t_star=0, master=NULL, outfile=NULL) {
  
  file_info <- list(dir=dir, tac_format=tac_format, 
                    tac_file_suffix=tac_file_suffix, vol_format=vol_format,
                    vol_file_suffix=vol_file_suffix)

  all_models <- names(model_definitions())
  if (!(all(models %in% all_models))) stop("Invalid model name(s) supplied.")
  
  for (this_model in models) {
    if (this_model == "Logan") {
      DVR <- model_batch(participants=participants, model="Logan", PVC=PVC,
                         file_info=file_info, ROI_def=ROI_def, k2prime=k2prime,
                         t_star=t_star, ref=ref, merge=merge)
                       
      names(DVR) <- lapply(names(DVR), paste, "_DVR", sep="")
      if (is.null(master)) master <- DVR else master <- data.frame(master, DVR)
    } else {
        MOD <- model_batch(participants=participants, model=this_model, 
                           PVC=PVC, SUVR_def=SUVR_def, ref=ref, merge=merge, 
                           file_info=file_info, ROI_def=ROI_def)
        names(MOD) <- lapply(names(MOD), paste, "_", this_model, sep="")
        if (is.null(master)) master <- MOD else master <- data.frame(master, 
                                                                     MOD)
    }
  }

  if (!(is.null(outfile))) write.csv(master, file = outfile)
  return(master)
}

#' Obtain values from voistat files (using load_voistat() for a batch.
#'
#' For a vector of participant IDs and correspondingly named .voistat files,
#' this extracts the value from the files for the specified ROIs.
#'
#' See load_voistat() for specifics.
#'
#'@param participants A vector of participant IDs
#'@param ROI_def Object that defines combined ROIs, see ROI_definitions.R
#'@param dir Directory and/or filename prefix of the files
#'@param filesuffix Optional filename characters between ID and ".voistat"
#'@param varname The name of the variable being exctracted, e.g. "SRTM"
#'@param otherdata A data.frame of the same participants to add the new data to
#'@param outfile Specify a filename to save the data
#'@return A table of values for the specified ROIs for all participants.
#'
voistat_batch <- function(participants, ROI_def, dir="", filesuffix, varname,
                         otherdata=NULL, outfile) {

  voistat_file = paste(dir, participants[1], filesuffix, ".voistat", sep="")

  first <- load_voistat(voistat_file, ROI_def)
  master <- t(first)
  master <- master[-1,]

  for (each in participants) {
    message(paste("Working on...", each))
    voistat_file = paste(dir, each, filesuffix, ".voistat", sep="")
    VALUE <- load_voistat(voistat_file, ROI_def)
    trans <- t(VALUE)
    row.names(trans) <- each
    master <- rbind(master,trans)
  }
  master <- as.data.frame(master)
  names(master) <- lapply(names(master), paste, "_", varname, sep="")
  if (!(is.null(otherdata))) master <- data.frame(otherdata, master)
  if (!(is.null(outfile))) write.csv(master, file = outfile)
  return(master)
}

# Counts ROIs in tac file of each listed participant; returns as dataframe.
QC_count_ROIs <- function(participants, tac_format="PMOD", dir="",
                          tac_file_suffix=".tac") {
  trip <- 0
  output <- data.frame(row.names=participants,
                       ROIs=rep(NA, length(participants)))
  for (each in participants) {
    message(paste("Working on...", each))
    tac_raw <- load_tac(paste(dir, each, tac_file_suffix, sep=""), 
                           tac_format)
    if (trip == 0) {
      headers <- names(tac_raw)
      trip <- 1
    } else {
        if (!all(names(tac_raw) == headers)) {
          warning(paste(each, ": ROIs do not match first participant."))
        }
      }
    
    output[each, ] <- length(tac_raw)
    
  }
    
  if ( (length(unique(output$ROIs))) > 1 ) {
    warning(paste("Unique ROI sets:", as.character(unique(output$ROIs))))
  }
    
  return(output)
}
