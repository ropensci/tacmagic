##################################
## PET Analysis in R            ##
## loading.R                    ##
## (C) Eric E. Brown  2018      ##
## PEAR v devel                 ##
## Beta version--check all work ##
##################################


# Loading utility functions

# BPnd data can be copied from PNEURO and saved as a CSV. It contains ROI volume
# information. This extracts that.
volumesFromBPndPaste <- function(BPnd_file) {
    BPnd <- read.csv(BPnd_file, header=TRUE, row.names=1)
    return(BPnd["Volume..ccm."])
}

# TAC .voistat files contain volume information for each ROI.
# This extracts that. Common time is simply a frame time that appears once in
# each ROI.
volumesFromVoistatTAC <- function(voistat_file, commontime="30") {
    voistat <- read.csv(voistat_file, sep="\t", skip=6, header=T,
    stringsAsFactors=F)
    # create a list of each unique ROI name
    ROIs <- unique(voistat[,"VoiName.Region...string."])
    # subset the voistat data to include just a single time frame, since
    # volume is same for each time
    u <- voistat$Time..seconds.==commontime
    # create a list of the volumes for each ROI.
    Volume..ccm. <- voistat[, "Volume..ccm."][u]
    return(data.frame(ROIs, Volume..ccm., row.names=1))
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

