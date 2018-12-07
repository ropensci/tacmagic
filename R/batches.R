##################################
## PET Analysis in R            ##
## batches.R                    ##
## (C) Eric E. Brown  2018      ##
## PEAR v devel                 ##
## Beta version--check all work ##
##################################


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
#'@param outputfilename Specify a filename to save the data.
#'@param corrected See calcSUVR() to determine whether this applies.
#'@return A table of SUVR values for the specified ROIs for all participants.
#'@examples 
#' batchSUVR(participants, ROI_def=standardROIs(),
#'           SUVR_def=c(3000, 3300, 3600, 3900), outputfilename="batch1.csv")
batchSUVR <- function(participants, dir="", tac_format="PMOD",
                      tac_file_suffix=".tac", vol_format="Voistat",
                      vol_file_suffix="_TAC.voistat", ROI_def, SUVR_def,
                      outputfilename) {
                          
  #Sets up the output file by using the first participant as a template.
  vol_file = paste(dir, participants[1], vol_file_suffix, sep="")
  vols <- loadVolumes(vol_file, format=vol_format)
  message("Loading first tac to create template table.")
  first_tac <- loadTACfile(paste(dir, participants[1], tac_file_suffix, sep=""),
                           tac_format)
  first <- calcSUVR(tac=first_tac, volumes=vols, ROI_def=ROI_def,
                    SUVR_def=SUVR_def)
  master <- t(first)
  master <- master[-1,]
  message("Empty master table complete; iterating through all participants.")
  # Runs through each participant to calculate the SUVR and store it.
  for (each in participants) {
    print(paste("Working on...", each))

    tac <- loadTACfile(paste(dir, each, tac_file_suffix, sep=""), tac_format)
    vols <- loadVolumes(paste(dir, each, vol_file_suffix, sep=""))
    
    SUVR <- calcSUVR(tac, vols, ROI_def, SUVR_def)
    trans <- t(SUVR)
    row.names(trans) <- each
    master <- rbind(master,trans)
  }
  # Save file and return the data.
  write.csv(master, file = outputfilename)
  return(as.data.frame(master))
}

# Batch as in calcSUVR but for the novel slope measure.
batchSlope <- function(participants, tac_format="PMOD", dir="",
                       tac_file_suffix=".tac", vol_format="Voistat",
                       vol_file_suffix="_TAC.voistat", ROI_def,
                       outputfilename) {
    
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
    
    # Save file and return the data.
    write.csv(master, file = outputfilename)
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
#'@param outputfilename Specify a filename to save the data.
#'@return A table of values for the specified ROIs for all participants.
#'@examples
#' batchVoistat(participants, ROI_def=standardROIs(), outputfilename="batch1.csv")
batchVoistat <- function(participants, ROI_def, outputfilename, dir="", filesuffix) {

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
  write.csv(master, file = outputfilename)
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
