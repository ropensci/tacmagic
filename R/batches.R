##################################
## PET Analysis in R            ##
## batches.R                    ##
## (C) Eric E. Brown  2018      ##
## PEAR v devel                 ##
## Beta version--check all work ##
##################################

#' Calculate multiple models for a batch of participants
#'
#' For a vector of participant IDs and correspondingly named tac and volume
#' files, this produces SUVR, DVR, and upslope in a tidy data.frame. Current
#' model options are "SUVR", "Logan" and "eslope".
#'
#' For further details about how the models are calculated, see the indiviudal
#' functions that they rely on. "SUVR" uses calcSUVR(), "Logan" uses
#' DVR_all_reference_Logan(), and "eslope" uses peaksSlope().
#'
#'@param participants A vector of participant IDs
#'@param models A vector of names of the models to calculate
#'@param tac_format Format of tac files provided: See loadTACfile()
#'@param dir A directory and/or file name prefix for the tac/volume files
#'@param tac_file_suffix How participant IDs corresponds to the TAC files
#'@param vol_format The file format that includes volumes: See loadVolumes()
#'@param vol_file_suffix How participant IDs correspond to volume files
#'@param ROI_def Object that defines combined ROIs, see ROI_definitions.R
#'@param SUVR_def is a vector of the start times for window to be used in SUVR
#'@param PVC For PVC, true where the data is stored as _C in same tac file
#'@param reference The name of the reference region for DVR/SUVR calculation
#'@param k2prime Fixed k2' for DVR calculation
#'@param t_star Change from 0 to manually specify a t* for DVR calculation
#'@param outfile Specify a filename to save the data
#'@return A table of SUVR values for the specified ROIs for all participants
#'@examples
#'
participant_batch <- function(participants, models=c("SUVR", "Logan", "eslope"),
                        dir="", tac_format="PMOD", tac_file_suffix=".tac",
                        vol_file_suffix="_TAC.voistat", vol_format="Voistat",
                        ROI_def, SUVR_def=NULL, PVC=F, reference="cerebellum",
                        k2prime=NULL, t_star=0, master=NULL, outfile=NULL) {
  
  all_models <- c("SUVR", "Logan", "eslope")
  if (!(all(models %in% all_models))) stop("Invalid model name(s) supplied.")
  
  if ("SUVR" %in% models) {
      # TODO check to ensure all required parameters are available
      SUVR <- batchSUVR(participants=participants, dir=dir,
                          tac_format=tac_format,
                          tac_file_suffix=tac_file_suffix,
                          vol_format=vol_format,
                          vol_file_suffix=vol_file_suffix,
                          ROI_def=ROI_def, SUVR_def=SUVR_def, PVC=PVC,
                          reference=reference, outfile=NULL)
     names(SUVR) <- lapply(names(SUVR), paste, "_SUVR", sep="")
     if (is.null(master)) master <- SUVR else master <- data.frame(master, SUVR)
  }
  
  if ("Logan" %in% models) {
      # TODO check to ensure all required parameters are available
      DVR <- batchDVR(participants, dir, tac_format, tac_file_suffix,
                      vol_format, vol_file_suffix, ROI_def, k2prime, t_star,
                      reference, PVC, outfile=NULL)
      names(DVR) <- lapply(names(DVR), paste, "_DVR", sep="")
      if (is.null(master)) master <- DVR else master <- data.frame(master, DVR)
  }

  if ("eslope" %in% models) {
      # TODO check to ensure all required parameters are available
      eslope <- batchSlope(participants, tac_format, dir, tac_file_suffix,
                           vol_format, vol_file_suffix, ROI_def,
                           outfile=NULL)
      names(eslope) <- lapply(names(eslope), paste, "_eslope", sep="")
      if (is.null(master)) master <- eslope else master <- data.frame(master,
                                                                      eslope)
  }
  if (!(is.null(outfile))) write.csv(master, file = outfile)
  return(master)
}

#' Obtain values from voistat files (using voistatScraper() for a batch.
#'
#' For a vector of participant IDs and correspondingly named .voistat files,
#' this extracts the value from the files for the specified ROIs.
#'
#' See voistatScraper() for specifics.
#'
#'@param participants A vector of participant IDs
#'@param ROI_def Object that defines combined ROIs, see ROI_definitions.R
#'@param dir Directory and/or filename prefix of the files
#'@param filesuffix Optional filename characters between ID and ".voistat"
#'@param otherdata A data.frame of the same participants to add the new data to
#'@param outfile Specify a filename to save the data.
#'@param varname The name of the variable being exctracted, e.g. "SRTM".
#'@return A table of values for the specified ROIs for all participants.
#'@examples
#'
batchVoistat <- function(participants, ROI_def, dir="", filesuffix, varname,
                         otherdata=NULL, outfile) {

  voistat_file = paste(dir, participants[1], filesuffix, ".voistat", sep="")

  first <- voistatScraper(voistat_file, ROI_def)
  master <- t(first)
  master <- master[-1,]

  for (each in participants) {
    message(paste("Working on...", each))
    voistat_file = paste(dir, each, filesuffix, ".voistat", sep="")
    VALUE <- voistatScraper(voistat_file, ROI_def)
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
        tac_raw <- loadTACfile(paste(dir, each, tac_file_suffix, sep=""),
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
