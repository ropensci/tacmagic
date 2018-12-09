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
                        k2prime=NULL, t_star=0, outfile=NULL) {
  
  all_models <- c("SUVR", "Logan", "eslope")
  if (!(all(models %in% all_models))) error("Invalid model name(s) supplied.")
  
  master <- NULL
  
  if ("SUVR" %in% models) {
      # TODO check to ensure all required parameters are available
      master <- batchSUVR(participants=participants, dir=dir, tac_format=tac_format, tac_file_suffix=tac_file_suffix,
                        vol_format=vol_format, vol_file_suffix=vol_file_suffix, ROI_def=ROI_def, SUVR_def=SUVR_def, PVC=PVC, reference=reference,
                        outfile=NULL)
  }
  
  if ("Logan" %in% models) {
      # TODO check to ensure all required parameters are available
      DVR <- batchDVR(participants, dir, tac_format, tac_file_suffix,
                      vol_format, vol_file_suffix, ROI_def, k2prime, t_star,
                      reference, PVC, outfile=NULL)
      if (is.null(master)) {
          master <- DVR
      } else {
          merge(master, DVR)
      }
  }

  if ("eslope" %in% models) {
      # TODO check to ensure all required parameters are available
      eslope <- batchSlope(participants, tac_format, dir, tac_file_suffix,
                           vol_format, vol_file_suffix, ROI_def,
                           outfile=NULL)
      if (is.null(master)) {
        master <- eslope
      } else {
        merge(master, eslope)
      }
  }
  if (!(is.null(outfile))) write.csv(master, file = outfile)
  return(master)
}


  
#' Calculate weighted SUVRs for specified ROIs in a participant batch
#'
#' For a vector of participant IDs and correspondingly named tac and volume 
#' files, this calculates SUVR using calcSUVR and stores in a single table.
#'
#' See calcSUVR() for how SUVR is calculated.
#'
#'@param participants A vector of participant IDs
#'@param tac_format See loadTACfile()
#'@param tac_file_suffix This is how the ID corresponds to the TAC files
#'@param vol_format See loadVolumes()
#'@param vol_file_suffix This is how the ID corresponds to the volume files
#'@param ROI_def Object that defines combined ROIs, see ROI_definitions.R
#'@param SUVR_def is a vector of the start times for window to be used in SUVR
#'@param corrected For PVC, true where the data is stored as _C in same tac file
#'@param outfile Specify a filename to save the data.
#'@param corrected See calcSUVR() to determine whether this applies.
#'@return A table of SUVR values for the specified ROIs for all participants.
#'@examples 
#' batchSUVR(participants, ROI_def=standardROIs(),
#'           SUVR_def=c(3000, 3300, 3600, 3900), outfile="batch1.csv")
batchSUVR <- function(participants, dir="", tac_format="PMOD",
                      tac_file_suffix=".tac", vol_format="Voistat",
                      vol_file_suffix="_TAC.voistat", ROI_def, SUVR_def,
                      reference, PVC, outfile=NULL) {
                          
  #Sets up the output file by using the first participant as a template.
  vol_file = paste(dir, participants[1], vol_file_suffix, sep="")
  vols <- loadVolumes(vol_file, format=vol_format)
  message("Loading first tac to create template table.")
  first_tac <- calcTAC(loadTACfile(paste(dir, participants[1], tac_file_suffix,
                                   sep=""), tac_format), volumes=vols, ROI_def=ROI_def, PVC=PVC)

  first <- calcSUVR(first_tac, SUVR_def=SUVR_def, reference=reference, ROI_def=ROI_def)
  master <- t(first)
  master <- master[-1,]
  message("Empty master table complete; iterating through all participants.")
  # Runs through each participant to calculate the SUVR and store it.
  for (each in participants) {
    message(paste("Working on...", each))

    vols <- loadVolumes(paste(dir, each, vol_file_suffix, sep=""))
    tac <- calcTAC(loadTACfile(paste(dir, each, tac_file_suffix, sep=""),
                   tac_format), vols, ROI_def)
    
    SUVR <- calcSUVR(tac, SUVR_def, reference, ROI_def=ROI_def)
    trans <- t(SUVR)
    row.names(trans) <- each
    master <- rbind(master,trans)
  }
  message("SUVR calculated for all participants.")
  if (!(is.null(outfile))) write.csv(master, file = outfile)
  return(as.data.frame(master))
}

batchDVR <- function(participants, dir="", tac_format="PMOD",
                      tac_file_suffix=".tac", vol_format="Voistat",
                      vol_file_suffix="_TAC.voistat", ROI_def, k2prime,
                      t_star=0, reference="cerebellum", PVC=F, outfile=NULL) {
                          
  #Sets up the output file by using the first participant as a template.
  vol_file = paste(dir, participants[1], vol_file_suffix, sep="")
  vols <- loadVolumes(vol_file, format=vol_format)
  message("Loading first tac to create DVR template table.")
  
  first_tac <- calcTAC(loadTACfile(paste(dir, participants[1], tac_file_suffix,
                    sep=""), tac_format), volumes=vols, ROI_def=ROI_def, PVC=PVC)
  
  first <- DVR_all_reference_Logan(first_tac, ref=reference, k2prime=k2prime, t_star=t_star)
  master <- t(first)
  master <- master[-1,]
  message("Empty master table complete; iterating through all participants.")
  # Runs through each participant to calculate the DVR and store it.
  for (each in participants) {
    message(paste("Working on...", each))
    
    vols <- loadVolumes(paste(dir, each, vol_file_suffix, sep=""))
    
    tac_data <- calcTAC(loadTACfile(paste(dir, each, tac_file_suffix,
                sep=""), tac_format), volumes=vols, ROI_def=ROI_def, PVC=PVC)
    
    DVR <- DVR_all_reference_Logan(tac_data, reference, k2prime, t_star)
    trans <- t(DVR)
    row.names(trans) <- each
    master <- rbind(master,trans)
  }
  
  if (!(is.null(outfile))) write.csv(master, file = outfile)
  return(as.data.frame(master))
}

                      
# Batch as in calcSUVR but for the novel slope measure.
batchSlope <- function(participants, tac_format="PMOD", dir="",
                       tac_file_suffix=".tac", vol_format="Voistat",
                       vol_file_suffix="_TAC.voistat", ROI_def, outfile=NULL) {
    
    #Sets up the output file by using the first participant as a template.
    vol_file = paste(dir, participants[1], vol_file_suffix, sep="")
    vols <- loadVolumes(vol_file, format=vol_format)
    message("Loading first tac to create template table.")
    first_tac_raw <- loadTACfile(paste(dir, participants[1], tac_file_suffix,
                                       sep=""), tac_format)
    first_tac <- calcTAC(first_tac_raw, vols, ROI_def=ROI_def)
    message("Loaded first tac file. Calculating SUVR...")
    
    first <- peakSlope(first_tac)
    message("First slopes calculated.")
    master <- t(first)
    master <- master[-1,]
    message("Empty master table complete; iterating through all participants.")
    
    # Runs through each participant to calculate the SUVR and store it.
    for (each in participants) {
        message(paste("Working on...", each))
        
        tac_raw <- loadTACfile(paste(dir, each, tac_file_suffix, sep=""),
                                     tac_format)
        vols <- loadVolumes(paste(dir, each, vol_file_suffix, sep=""),
                                  format=vol_format)
        tac <- calcTAC(tac_raw, vols, ROI_def=ROI_def)
        
        SLOPE <- peakSlope(tac)
        trans <- t(SLOPE)
        row.names(trans) <- each
        master <- rbind(master,trans)
    }
    
    if (!(is.null(outfile))) write.csv(master, file = outfile)
    return(as.data.frame(master))
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
#'@param outfile Specify a filename to save the data.
#'@return A table of values for the specified ROIs for all participants.
#'@examples
#' batchVoistat(participants, ROI_def=standardROIs(), outfile="batch1.csv")
batchVoistat <- function(participants, ROI_def, outfile, dir="", filesuffix) {

  voistat_file = paste(dir, participants[1], filesuffix, ".voistat", sep="")

  first <- voistatScraper(voistat_file, ROI_def)
  master <- t(first)
  master <- master[-1,]

  for (each in participants) {
    print(paste("Working on...", each))
    voistat_file = paste(dir, each, filesuffix, ".voistat", sep="")
    VALUE <- voistatScraper(voistat_file, ROI_def)
    trans <- t(VALUE)
    row.names(trans) <- each
    master <- rbind(master,trans)
  }
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
