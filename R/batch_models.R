##################################
## PET Analysis in R            ##
## batch_models.R               ##
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
#'@param dir A directory and/or file name prefix for the tac/volume files
#'@param tac_format See loadTACfile()
#'@param tac_file_suffix This is how the ID corresponds to the TAC files
#'@param vol_format See loadVolumes()
#'@param vol_file_suffix This is how the ID corresponds to the volume files
#'@param ROI_def Object that defines combined ROIs, see ROI_definitions.R
#'@param SUVR_def is a vector of the start times for window to be used in SUVR
#'@param reference The name of the reference region for DVR/SUVR calculation
#'@param PVC For PVC, set to T where the data is stored as _C in same tac file
#'@param outfile Specify a filename to save the data.
#'@return A table of SUVR values for the specified ROIs for all participants.
#'@examples
#'
batchSUVR <- function(participants, dir="", tac_format="PMOD",
                      tac_file_suffix=".tac", vol_format="Voistat",
                      vol_file_suffix="_TAC.voistat", ROI_def, SUVR_def,
                      reference, PVC, outfile=NULL) {
    
    # Generate the file names
    tac_files <- paste(dir, participants, tac_file_suffix, sep="")
    vol_files <- paste(dir, participants, vol_file_suffix, sep="")
    
    #Sets up the output file by using the first participant as a template.
    
    message("Loading first tac to create template table.")
    vol1 <- loadVolumes(vol_files[1], format=vol_format)
    tac1 <- loadTACfile(tac_files[1], tac_format)
    first_tac <- calcTAC(tac1, volumes=vol1, ROI_def=ROI_def, PVC=PVC)
    
    first <- calcSUVR(first_tac, SUVR_def=SUVR_def, reference=reference,
                      ROI_def=ROI_def)
    master <- t(first)
    master <- master[-1,]
    message("Empty master table complete; iterating through all participants.")
    # Runs through each participant to calculate the SUVR and store it.
    for (i in 1:length(participants)) {
        message(paste("Working on...", participants[i]))
        
        voli <- loadVolumes(vol_files[i], vol_format)
        taci <- loadTACfile(tac_files[i], tac_format)
        tac_data <- calcTAC(taci, voli, ROI_def=ROI_def, PVC=PVC)
        
        SUVR <- calcSUVR(tac_data, SUVR_def, reference, ROI_def=ROI_def)
        trans <- t(SUVR)
        row.names(trans) <- participants[i]
        master <- rbind(master,trans)
    }
    message("SUVR calculated for all participants.")
    if (!(is.null(outfile))) write.csv(master, file = outfile)
    return(as.data.frame(master))
}

#' Calculate weighted SUVRs for specified ROIs in a participant batch
#'
#' For a vector of participant IDs and correspondingly named tac and volume
#' files, this calculates SUVR using calcSUVR and stores in a single table.
#'
#' See calcSUVR() for how SUVR is calculated.
#'
#'@param participants A vector of participant IDs
#'@param dir A directory and/or file name prefix for the tac/volume files
#'@param tac_format See loadTACfile()
#'@param tac_file_suffix This is how the ID corresponds to the TAC files
#'@param vol_format See loadVolumes()
#'@param vol_file_suffix This is how the ID corresponds to the volume files
#'@param ROI_def Object that defines combined ROIs, see ROI_definitions.R
#'@param SUVR_def is a vector of the start times for window to be used in SUVR
#'@param reference The name of the reference region for DVR/SUVR calculation
#'@param PVC For PVC, set to T where the data is stored as _C in same tac file
#'@param outfile Specify a filename to save the data.
#'@return A table of SUVR values for the specified ROIs for all participants.
#'@examples
#'
batchDVR <- function(participants, dir="", tac_format="PMOD",
                     tac_file_suffix=".tac", vol_format="Voistat",
                     vol_file_suffix="_TAC.voistat", ROI_def, k2prime,
                     t_star=0, reference="cerebellum", PVC, outfile=NULL) {
    
    # Generate the file names
    tac_files <- paste(dir, participants, tac_file_suffix, sep="")
    vol_files <- paste(dir, participants, vol_file_suffix, sep="")
    
    #Sets up the output file by using the first participant as a template.
    message("Loading first tac to create DVR template table.")
    
    vol1 <- loadVolumes(vol_files[1], format=vol_format)
    tac1 <- loadTACfile(tac_files[1], tac_format)
    first_tac <- calcTAC(tac1, volumes=vol1, ROI_def=ROI_def, PVC=PVC)
    
    first <- DVR_all_reference_Logan(first_tac, ref=reference, k2prime=k2prime,
                                     t_star=t_star)
    master <- t(first)
    master <- master[-1,]
    message("Empty master table complete; iterating through all participants.")
    
    # Runs through each participant to calculate the DVR and store it.
    for (i in 1:length(participants)) {
        message(paste("Working on...", participants[i]))
        
        voli <- loadVolumes(vol_files[i], format=vol_format)
        taci <- loadTACfile(tac_files[i], tac_format)
        tac_data <- calcTAC(taci, volumes=voli, ROI_def=ROI_def, PVC=PVC)
        
        DVR <- DVR_all_reference_Logan(tac_data, reference, k2prime, t_star)
        trans <- t(DVR)
        row.names(trans) <- participants[i]
        master <- rbind(master,trans)
    }
    
    if (!(is.null(outfile))) write.csv(master, file = outfile)
    return(as.data.frame(master))
}


# Batch as in calcSUVR but for the novel slope measure.
batchSlope <- function(participants, tac_format="PMOD", dir="",
                       tac_file_suffix=".tac", vol_format="Voistat",
                       vol_file_suffix="_TAC.voistat", ROI_def, outfile=NULL,
                       PVC) {
    
    # Generate the file names
    tac_files <- paste(dir, participants, tac_file_suffix, sep="")
    vol_files <- paste(dir, participants, vol_file_suffix, sep="")
    
    #Sets up the output file by using the first participant as a template.
    vol1 <- loadVolumes(vol_files[1], format=vol_format)
    message("Loading first tac to create template table.")
    tac1 <- loadTACfile(tac_files[1], tac_format)
    first_tac <- calcTAC(tac1, volumes=vol1, ROI_def=ROI_def, PVC=PVC)
    message("Loaded first tac file. Calculating SUVR...")
    
    first <- peakSlope(first_tac)
    message("First slopes calculated.")
    master <- t(first)
    master <- master[-1,]
    message("Empty master table complete; iterating through all participants.")
    
    # Runs through each participant to calculate the SUVR and store it.
    for (i in 1:length(participants)) {
        message(paste("Working on...", participants[i]))
        
        taci <- loadTACfile(tac_files[i], tac_format)
        voli <- loadVolumes(vol_files[i], format=vol_format)
        tac_data <- calcTAC(taci, voli, ROI_def=ROI_def, PVC=PVC)
        
        SLOPE <- peakSlope(tac_data)
        trans <- t(SLOPE)
        row.names(trans) <- participants[i]
        master <- rbind(master,trans)
    }
    
    if (!(is.null(outfile))) write.csv(master, file = outfile)
    return(as.data.frame(master))
}
