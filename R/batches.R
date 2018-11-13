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
#' batchSUVR(participants, ROI_def=standardROIs(), SUVR_def=c("3000", "3300", "3600", "3900"), outputfilename="batch1.csv")
batchSUVR <- function(participants, tac_format="PMOD", tac_file_suffix=".tac",
                      vol_format="Voistat", vol_file_suffix="_TAC.voistat",
                      ROI_def, SUVR_def, outputfilename, corrected=FALSE) {
                          
  #Sets up the output file by using the first participant as a template.
  vol_file = paste(participants[1], vol_file_suffix, sep="")
  vols <- loadVolumes(vol_file, format=vol_format)
  print("Loading first tac to create template table.")
  first_tac <- loadTACfile(paste(participants[1], tac_file_suffix, sep=""), tac_format)
  print("Loaded first tac file. Calculating SUVR...")
  first <- calcSUVR(tac=first_tac, volumes=vols, ROI_def=ROI_def, SUVR_def=SUVR_def, corrected=corrected)
  print("First SUVRs calculated.")
  master <- t(first)
  master <- master[-1,]
  print("Empty master table complete; iterating through all participants.")
  # Runs through each participant to calculate the SUVR and store it.
  for (each in participants) {
    print(paste("Working on...", each))

    tac <- loadTACfile(paste(each, tac_file_suffix, sep=""), tac_format)
    vols <- loadVolumes(paste(each, vol_file_suffix, sep=""))
    
    SUVR <- calcSUVR(tac, vols, ROI_def, SUVR_def, corrected)
    trans <- t(SUVR)
    row.names(trans) <- each
    master <- rbind(master,trans)
  }
  
  # Save file and return the data.
  write.csv(master, file = outputfilename)
  return(master)
}

#Batch slope
batchSlope <- function(participants, ROI_def, outputfilename, corrected=TRUE,
                       volfromBPnd=FALSE, tacfilesuffix=".tac") {

  vols <- calcRelativeVolumes(loadVolumes(BPnd_file), ROI_def)

  TAC_file = paste(participants[1], tacfilesuffix, sep="")
  
  cat("Current TAC file:", TAC_file)

  firstslope <- peakSlope(TAC_file)
  first <- peakSlopeROI(firstslope, ROI_def, vols, corrected)
  master <- t(first)
  master <- master[-1,]
  
  for (each in participants) {
    print(paste("Working on...", each))
    TAC_file = paste(each, tacfilesuffix, sep="")
  
    vols <- calcRelativeVolumes(loadVolumes(BPnd_file), ROI_def)
   
    BPnd_file = paste(each, "_BPnd.csv", sep="")
    
    slope <- peakSlope(TAC_file)
    slopeROI <- peakSlopeROI(slope, ROI_def, vols, corrected)

    trans <- t(slopeROI)
    row.names(trans) <- each
    master <- rbind(master,trans)
  }
  write.csv(master, file = outputfilename)
  return(master)
}

#A function to run voistatScraper on a list of participants
batchVoistat <- function(participants, ROI_def=standardROIs(), outputfilename,
                         filesuffix) {

  voistat_file = paste(participants[1], filesuffix, ".voistat", sep="")

  first <- voistatScraper(voistat_file, ROI_def)
  master <- t(first)
  master <- master[-1,]

  for (each in participants) {
    print(paste("Working on...", each))
    voistat_file = paste(each, filesuffix, ".voistat", sep="")
    VALUE <- voistatScraper(voistat_file, ROI_def)
    trans <- t(VALUE)
    row.names(trans) <- each
    master <- rbind(master,trans)
  }
  write.csv(master, file = outputfilename)
  return(master)
}
