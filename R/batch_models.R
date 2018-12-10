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
    first_tac <- calcTAC(loadTACfile(paste(dir, participants[1],
                                           tac_file_suffix, sep=""),
                                     tac_format), volumes=vols, ROI_def=ROI_def,
                                     PVC=PVC)
    
    first <- calcSUVR(first_tac, SUVR_def=SUVR_def, reference=reference,
                      ROI_def=ROI_def)
    master <- t(first)
    master <- master[-1,]
    message("Empty master table complete; iterating through all participants.")
    # Runs through each participant to calculate the SUVR and store it.
    for (each in participants) {
        message(paste("Working on...", each))
        
        vols <- loadVolumes(paste(dir, each, vol_file_suffix, sep=""))
        tac <- calcTAC(loadTACfile(paste(dir, each, tac_file_suffix, sep=""),
        tac_format), vols, ROI_def, PVC=PVC)
        
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
    
    first <- DVR_all_reference_Logan(first_tac, ref=reference, k2prime=k2prime,
    t_star=t_star)
    master <- t(first)
    master <- master[-1,]
    message("Empty master table complete; iterating through all participants.")
    # Runs through each participant to calculate the DVR and store it.
    for (each in participants) {
        message(paste("Working on...", each))
        
        vols <- loadVolumes(paste(dir, each, vol_file_suffix, sep=""))
        
        tac_data <- calcTAC(loadTACfile(paste(dir, each, tac_file_suffix,
        sep=""),
        tac_format), volumes=vols, ROI_def=ROI_def,
        PVC=PVC)
        
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
                       vol_file_suffix="_TAC.voistat", ROI_def, outfile=NULL,
                       PVC) {
    
    #Sets up the output file by using the first participant as a template.
    vol_file = paste(dir, participants[1], vol_file_suffix, sep="")
    vols <- loadVolumes(vol_file, format=vol_format)
    message("Loading first tac to create template table.")
    first_tac_raw <- loadTACfile(paste(dir, participants[1], tac_file_suffix,
    sep=""),
    tac_format)
    first_tac <- calcTAC(first_tac_raw, vols, ROI_def=ROI_def, PVC=PVC)
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
        tac <- calcTAC(tac_raw, vols, ROI_def=ROI_def, PVC=PVC)
        
        SLOPE <- peakSlope(tac)
        trans <- t(SLOPE)
        row.names(trans) <- each
        master <- rbind(master,trans)
    }
    
    if (!(is.null(outfile))) write.csv(master, file = outfile)
    return(as.data.frame(master))
}
