##################################
## PET Analysis in R            ##
## loading_utilities.R          ##
## (C) Eric E. Brown  2018      ##
## PEAR v devel                 ##
## Beta version--check all work ##
##################################


# Loading utility functions


## VOLUME INFORMATION

# BPnd data can be copied from PNEURO and saved as a CSV. It contains ROI volume
# information. This extracts that.
volumesFromBPndPaste <- function(BPnd_file) {
    BPnd <- read.csv(BPnd_file, header=TRUE, row.names=1)
    return(BPnd["Volume..ccm."])
}

# TAC .voistat files contain volume information for each ROI. This extracts it. 
volumesFromVoistatTAC <- function(voistat_file) {
    voistat <- read.csv(voistat_file, sep="\t", skip=6, header=T,
    stringsAsFactors=F)
    # create a list of each unique ROI name
    ROIs <- unique(voistat[,"VoiName.Region...string."])
    # subset the voistat data to include just a single time frame, since
    # volume is same for each time
    u <- voistat$Time..seconds.==voistat$Time..seconds.[1]
    # create a list of the volumes for each ROI.
    Volume..ccm. <- voistat[, "Volume..ccm."][u]
    return(data.frame(ROIs, Volume..ccm., row.names=1))
}



## TAC INFORMATION

loadTACvoistat <- function(voistat_file) {
  voistat <- read.csv(voistat_file, sep="\t", skip=6, header=T,
  stringsAsFactors=F)

  voistat_type <- validateTACvoistat(voistat)
  
  if (voistat_type == "invalid") error("Invalid voistat TAC file.")

  ROIs <- unique(test$VoiName.Region...string.)

  variables <- c("time", ROIs)
  if (voistat_type == "C") variables <- c(variables, paste(ROIs, "_C", sep=""))

  frames <- length(voistat[voistat$VoiName.Region...string. == ROIs[3], ][,1])

  tac <- as.data.frame(matrix(ncol=length(variables), nrow=frames))
  names(tac) <- variables

  tac$time <- voistat[voistat$VoiName.Region...string. == ROIs[3], ][, "Time..seconds."]

  for (i in 1:length(ROIs)) {
    tac[,ROIs[i]] <- voistat[voistat$VoiName.Region...string. == ROIs[i], ][, "Averaged..kBq.cc."]

    if (voistat_type == "C") {
      tac[,paste(ROIs[i], "_C", sep="")] <- voistat[voistat$VoiName.Region...string. == ROIs[i], ][, "PVC..kBq.cc."]
    }
  }
  return(tac)
}

validateTACtable <- function(tac) {
  # Checks to ensure there are start and stop times in the first 2 columns.  
  if (FALSE == (startsWith(names(tac)[1], "start") && 
                startsWith(names(tac)[2], "end"))) {
    stop("The first two columns of the TAC file should be start and end times, 
          with headers starting with 'start' and 'end'.")
  }
  return(TRUE)
}

# Checks the headers of the TAC voistat file to ensure they are as expected,
# and returns C if there is a PVC value, NC if not, and invalid if headers
# are not as expected.
validateTACvoistat <- function(voistat) {

  PVC <- c("X...Component..string.", "File..string.", "PatientName..string.", 
           "PatientID..string.", "PatientInfo..string.", 
           "StudyDescription..string.", "SeriesDescription..string.", 
           "StudyDate..date_time.", "Color..0xARGB.", 
           "VoiName.Region...string.", "Time..seconds.", "Averaged..kBq.cc.", 
           "PVC..kBq.cc.", "Volume..ccm.")

  if (all(names(voistat) == PVC)) type <- "C" 
  else if (all(names(voistat) == PVC[-13])) type <- "NC" 
  else type <- "invalid"

  return(type)
}

