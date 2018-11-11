##################################
## PET Analysis in R            ##
## batches.R                    ##
## Eric E. Brown                ##
## PEAR v 0.1.8                 ##
## Beta version--check all work ##
##################################

source("calculateSUVR.R")
source("fullTAC.R")
source("utilities.R")
source("ROI_definitions.R")
source("loading.R")

# This function runs the SUVR calculation on a list of participants specified in
# a vector of participant IDs, where there are corresponding files named by
# ID with a specified suffix. It returns the data frame and also saves a CSV
# file (name in arguments).
batchSUVR <- function(participants, tac_format="PMOD", tac_file_suffix=".tac",
                      vol_format="Voistat", vol_file_suffix="_TAC.voistat",
                      ROI_def, SUVR_def, outputfilename, corrected=TRUE) {
                          
  #Sets up the output file by using the first participant as a template.
  vol_file = paste(participants[1], vol_file_suffix, sep="")
  vols <- calcRelativeVolumes(loadVolumes(vol_file, format=vol_format),
                              ROI_def)

  TAC_file = paste(participants[1], tac_file_suffix, sep="")
  
  first <- calcSUVR(TAC_file, ROI_def, vols, SUVR_def, corrected)
  master <- t(first)
  master <- master[-1,]

  # Runs through each participant to calculate the SUVR and store it.
  for (each in participants) {
    print(paste("Working on...", each))

    tac <- loadTACfile(paste(each, tac_file_suffix, sep=""), tac_format)
    vols <- calcRelativeVolumes(loadVolume(paste(each, vol_file_suffix, sep="")), ROI_def)
    
    SUVR <- calcSUVR(tac, ROI_def, vols, SUVR_def, corrected)
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

  if (volfromBPnd) {
    BPnd_file = paste(participants[1], "_BPnd.csv", sep="")
    vols <- calcRelativeVolumes(volumesFromBPndPaste(BPnd_file), ROI_def)
  }  else {
    voistat_file = paste(participants[1], ".voistat", sep="")
    vols <- calcRelativeVolumes(volumesFromVoistatTAC(voistat_file), ROI_def)
    }
  TAC_file = paste(participants[1], tacfilesuffix, sep="")
  
  cat("Current TAC file:", TAC_file)

  firstslope <- peakSlope(TAC_file)
  first <- peakSlopeROI(firstslope, ROI_def, vols, corrected)
  master <- t(first)
  master <- master[-1,]
  
  for (each in participants) {
    print(paste("Working on...", each))
    TAC_file = paste(each, tacfilesuffix, sep="")
  
    if (volfromBPnd) {
    BPnd_file = paste(each, "_BPnd.csv", sep="")
    vols <- calcRelativeVolumes(volumesFromBPndPaste(BPnd_file), ROI_def)
    }  else {
      voistat_file = paste(each, ".voistat", sep="")
      vols <- calcRelativeVolumes(volumesFromVoistatTAC(voistat_file), ROI_def)
      }
    
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
